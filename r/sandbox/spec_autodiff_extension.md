# Autodiff expansion plan

The expansion path is about lowering more recorded plans into the shared JAX
pipeline. Three items, in recommended order:

1. Pure comparison ops (lnratio, lnor, lift, and avg/wts variants)
2. Slopes (dydx) first, then built-in elasticities (eyex, eydx, dyex)
3. Linear formula hypotheses lowered to contrast matrices

Every item benefits from the existing guardrail in `autodiff_try()` (both
languages): JAX estimates are compared against the standard pipeline with a
1e-8 tolerance, and any mismatch reverts to finite differences with a warning.
A lowering bug degrades to "no speedup," not wrong numbers.

---

## 1. Pure comparison ops

Add `lnratio`, `lnor`, `lift`, plus `lnratioavg`, `lnoravg`, `liftavg` and the
`*avgwts` weighted variants. These need only `hi`, `lo`, and optional `w` —
no new pipeline inputs.

**Files:**
- `python/marginaleffects/autodiff/ops.py` — op definitions and registries
- `python/marginaleffects/autodiff/lower.py` — no changes expected (validates via `COMPARISON_OPS` membership)
- `r/R/autodiff_lower.R` — `autodiff_op_registry` entries

**Reference implementations to match exactly** (from
`python/marginaleffects/estimands.py` and `r/R/sanitize_comparison.R`):

```
lnratio      = log(hi / lo)                                  elementwise
lnratioavg   = log(wmean(hi, w) / wmean(lo, w))              scalar
lnor         = log((hi/(1-hi)) / (lo/(1-lo)))                elementwise
lnoravg      = log(odds(wmean(hi,w)) / odds(wmean(lo,w)))    scalar
lift         = (hi - lo) / lo                                elementwise
liftavg      = (wmean(hi-lo, w)) / wmean(lo, w)              scalar
```

**Implementation steps:**

1. In `ops.py`, add one estimate function per op to `PIPELINE_OPS`. The
   current `EstimateFn` signature is `(hi, lo, w, array, wmean)` with helpers
   injected so `ops.py` never imports JAX (it must stay importable without
   JAX for `lower.py`'s fallback path). `log` is not currently injected: either
   extend the injected helper set (add a `log` argument alongside `array` and
   `wmean`) or pass a namespace module (`xp=jnp`) from `pipeline.py`. Prefer
   the namespace approach; it also covers item 2's needs.
2. Add `COMPARISON_OPS` entries mapping each `fun_key` to its pipeline op,
   with `weighted=True` for the `*avgwts` variants (mirrors the existing
   `differenceavgwts` → `differenceavg` pattern).
3. In `r/R/autodiff_lower.R`, add the same `fun_key → pipeline op` mappings to
   `autodiff_op_registry` (e.g. `lnratioavgwts = "lnratioavg"`). The existing
   `autodiff_op_weights()` already handles `wts$`-suffixed keys.
4. No changes to `pipeline.py` beyond the helper injection: `_comparison_estimates`
   dispatches through `PIPELINE_OPS`, and scalar/elementwise output sizing is
   already handled by `PipelineOp.output_size()`.

**Domain note:** `lnor` is only meaningful when predictions are probabilities,
and `log` ops produce NaN/±Inf outside their domain. No special handling is
needed — the estimate-equality guardrail catches any divergence and falls back.

**Exclusion:** `expdydx*` looks like a comparison op but requires `eps`; it
belongs with item 2.

**Tests:** for each op, paired autodiff-off/on runs asserting estimate
equality and SE agreement with finite differences (rtol ~1e-6); one lm and one
glm (logit) model each; a weighted-`by` case for the `wts` variants. Run via
`make py-test-autodiff` and `make r-autodiff`.

---

## 2. Slopes and elasticities

Split in two stages because `dydx` is much cheaper than the true elasticities.

### 2a. dydx (highest user value)

`dydx = (hi - lo) / eps` is just a scaled difference. It does not set
`plan.need_y` (only `eyex`/`eydx`/`dyex` and custom functions do), so the
current rejection is purely the registry check. This unlocks `slopes()` /
`avg_slopes()`, the flagship calls, where large-N finite-difference Jacobians
hurt most.

**Ops to add:** `dydx`, `dydxavg`, `dydxavgwts`.

**New pipeline input — per-group `eps`:**
- Python: global `plan.eps` on `ComparisonPlan`; R: per-group `g$args$eps`
  (set from `variables[[tn]]$eps` in `comparison_plan.R`). Lower both to a
  per-op scalar in the ops dicts: `{"op": ..., "n": ..., "w": ..., "eps": ...}`.
  Fail lowering if `eps` is missing or NA.
- In `pipeline.py`, do not put `eps` in the static `ops_meta` tuple (a new
  float value would trigger recompilation). Pass it as a traced scalar in a
  tuple parallel to `ops_weights` (e.g. `ops_params`), and record only its
  presence (a boolean) in `ops_meta`.
- Extend the `EstimateFn` signature to receive `eps` (or a params dict) — this
  is the same signature change as the `xp` namespace in item 1, so do them
  together.

### 2b. Built-in elasticities (eyex, eydx, dyex)

Reference implementations (`estimands.py`):

```
eyex = (hi - lo) / eps * (x / y)
eydx = ((hi - lo) / eps) / y
dyex = ((hi - lo) / eps) * x
```

plus `*avg` / `*avgwts` variants. These set `plan.need_y`, so the lowerers'
`need_y` rejection is replaced by explicit support:

**New pipeline inputs:**
- `X_nd`: design matrix for baseline predictions on the original data, so the
  pipeline computes `y = f(X_nd @ beta)` inside the traced function — the
  Jacobian must flow through `y`, since it depends on `beta`.
  - Python: build from `plan.exog_nd` (already on `ComparisonPlan`), validated
    with `_design_or_failure` like `exog_hi`/`exog_lo`.
  - R: model matrix attribute on `original` (same
    `marginaleffects_model_matrix` mechanism as `hi`/`lo`); fail lowering if
    absent. Apply the same `na_keep`/`perm` row filters as `X_hi`/`X_lo`.
- Per-group `x`: regressor values. Python: `CompGroup.x`; R: `g$args$x`.
  Traced arrays, passed like weights.
- **Alignment:** `lower_comparisons` permutes `X_hi[order]`/`X_lo[order]` into
  group-contiguous order. `X_nd` rows and `x` values must be permuted
  identically, or group slices of `y` will not line up with `hi`/`lo`.

**Scope:** built-ins only. Custom comparison functions (`fun_key` None/NA)
keep falling back. `expdydx*` can ride along here since it only needs `eps`.

**Tests:** as in item 1, plus `slopes()`/`avg_slopes()` round-trips with
`slope="eyex"` etc., and an explicit fallback test for a custom comparison
function.

---

## 3. Linear formula hypotheses → matrix

Matrix hypotheses already lower for free: both lowerers accept
`kind = "matrix"` and the pipeline applies `est @ H` inside the traced
function, so the Jacobian transform `J_hyp = H.T @ J_est` comes out of
autodiff automatically. Formula hypotheses that are fixed linear contrasts can
be converted to matrices at compile time, keeping the whole pipeline on the
supported path.

**Good candidates** (fixed linear contrast weights):

- `difference ~ reference` / `revreference`
- `difference ~ sequential`
- `difference ~ pairwise` / `revpairwise`
- `difference ~ meandev` / `meanotherdev`
- `difference ~ trt_vs_ctrl`
- `dotproduct ~ poly` / `helmert`

**Not eligible** (transform depends on estimate values): `ratio ~ *`,
arbitrary `I(...)` functions. These keep the current `kind = "formula"`
fallback.

**Where:**
- R: `hypothesis_compile_formula()` in `r/R/hypothesis_compile.R`, with the
  per-rhs comparison functions in `r/R/hypothesis_formula.R`
  (`hypothesis_formula_list`).
- Python: `hypothesis_compile()` / `_compile_formula_hypothesis()` in
  `python/marginaleffects/hypothesis_compile.py`.

**Implementation:**

1. Whitelist eligible `(lhs, rhs)` pairs (the list above). Everything else
   keeps the current formula path unchanged.
2. Recover `H` by probing the existing comparison function with unit vectors
   rather than writing per-rhs H builders: for a linear map
   `f: R^k → R^m`, column `j` of `A` is `f(e_j)`, and `H = A.T` under the
   `est @ H` convention. This reuses `hypothesis_formula_list` /
   `eval_hypothesis_formula` as the single source of truth for contrast
   definitions and row order.
3. Grouped formulas (`lhs ~ rhs | group`): `hypothesis_compile_formula()`
   already builds the per-group index lists (`groups`). Build one `H_g` per
   group by probing within the group, then assemble a single
   `n_est × n_out` matrix with each block's rows placed at that group's
   estimate indices — block structure preserves current output order and
   labels.
4. Verify before committing: check `f(est) == est @ H` on the baseline
   estimates (and optionally linearity on a random draw:
   `f(x + y) == f(x) + f(y)`). On any mismatch, silently keep
   `kind = "formula"`. This makes the conversion behavior-preserving by
   construction.
5. Return the same `cmp` output (labels computed exactly as today) but with
   `hyp = list(kind = "matrix", apply = ..., H = H)`. Preserve the
   `hypothesis_function_by` attribute (R). No changes needed in
   `autodiff_lower.R` / `lower.py` — they already accept matrix hypotheses.

**Caveats:**
- The pairwise safe-mode guardrail (>25 estimates) still applies because the
  existing functions are called during probing; probing cost is `k` calls to a
  cheap vector function, negligible next to prediction.
- Performance framing: this mostly does not reduce the cost of the base
  Jacobian `J_est`. The win is (a) keeping autodiff available at all when a
  formula hypothesis would otherwise force finite differences, and (b)
  replacing R/Python-level formula evaluation with a matrix product in the
  compiled graph. Biggest gains: large pairwise/grouped contrasts.

**Tests:** for each whitelisted pair — estimates and labels identical to the
formula path with autodiff off; SEs match finite differences with autodiff on;
one grouped (`| group`) case; explicit fallback tests for `ratio ~ pairwise`
and an `I(...)` formula.

---

## Testing recipe (applies to every item)

Each expansion ships with paired tests:
1. autodiff disabled vs enabled on the same call,
2. estimate equality (exact, since the pipeline guarantees it),
3. SE agreement within tolerance vs finite differences,
4. an explicit fallback test for a neighboring unsupported variant, asserting
   the "Reverting to finite differences" warning fires with the right reason.
