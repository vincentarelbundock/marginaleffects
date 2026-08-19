# SPEC: bring the Python Jacobian architecture to parity with R

Status: implemented and verified on the Python test suite.
Date: 2026-08-19
Scope: `python/marginaleffects/planning/`, `python/marginaleffects/inference/`,
`python/marginaleffects/classes/model.py`, `python/marginaleffects/statsmodels/model.py`,
`python/marginaleffects/hypothesis_compile.py`.

## Implementation status

Python now composes the coefficient Jacobian through prediction, comparison,
aggregation, and hypothesis stages. The implementation includes:

- exact gradients for all recorded built-in comparison functions;
- response-scale GLM derivatives from adapter-provided inverse-link functions;
- sparse grouped aggregation and contraction before materialization where possible;
- exact matrices for structurally linear string and formula hypotheses, with a
  bounded dense probe for nonlinear hypotheses;
- unique coefficient/design-name validation and design-column reordering;
- unconditional fallback for custom comparison stages without an explicit,
  trusted derivative;
- element-wise replay, missing-value, offset/exposure, and whole-Jacobian
  finiteness guards; and
- public `jacobian_method` result metadata with `analytic`,
  `analytic+numeric_stage`, `autodiff`, and `finite_difference` values.

The Python comparison plan represents comparison-level aggregation inside its
`CompGroup` objects rather than adding a second `ComparisonPlan.agg` layer. This
is the branch's native equivalent of R's recorded aggregation stage and avoids
applying an average twice.

## 1. What parity means

R now treats every estimand as a pipeline and differentiates it stage by stage:

```
beta -> eta = X beta -> predictions -> comparison -> aggregation -> hypothesis
```

The Jacobian of the pipeline is the product of the stage Jacobians. The first
link is the only one that touches the model and the full data; every later stage
is cheap arithmetic on numbers already computed. Two consequences drive the whole
design:

- **Cost.** Differentiating the composition as a whole forces one model
  evaluation per coefficient. Composing instead costs one model matrix.
- **Accuracy.** A stage that averages `n` predictions carries roundoff of order
  `n * eps`. Dividing that by a coefficient-scaled finite-difference step
  amplifies it by orders of magnitude. Composing keeps the model evaluations on
  the well-scaled quantity.

The critical property is that an **exact first-stage Jacobian survives an opaque
downstream stage**. When a later stage has no closed form it is differentiated
numerically, but only ever through its own arithmetic — never by re-running the
model.

Python today has no stage composition. `analytic_try()` is all-or-nothing: one
unrecognized group key, one non-matrix hypothesis, one GLM, and the entire
pipeline reverts to finite differences over `beta`. Parity means replacing that
with composition.

## 2. Original Python state

Verified by reading the source, not inferred.

| Concern | R | Python today |
|---|---|---|
| Stage seam | `apply_plan_stages()` returns `(pre, post)` | `*_plan_apply()` returns one array; the value feeding the hypothesis is unobservable |
| Composition | `jacobian_stage.R` | absent |
| Comparison keys with closed forms | 22 | 6: `difference`, `ratio`, and their `avg` / `avgwts` |
| Response scale | `mu_eta` folded into the stage gradient | `_linear_design()` requires `model_type == "linear"`; **every GLM falls back** |
| Hypothesis kinds reaching the Jacobian | `matrix`, plus `string` / `formula` promoted to matrix, plus a dense probe for the rest | `matrix` only; `string`, `formula`, `list` return `None` |
| Comparison aggregation (`by=`) | recorded as `plan$agg` | `ComparisonPlan` **has no `agg` field**; `get_by_plan()` builds `AggGroup`s but only `PredictionPlan` stores them |
| `uses_y` | recorded per group | plan-level `need_y` only |
| Contraction | `crossprod()`, never forming the n x p it would discard | `_weighted_rows()` builds the full difference first |
| Non-finite guard | `matrix_all_finite()` | absent |
| Replay guard | element-wise | **already element-wise** (`np.allclose`) — at parity, keep it |

## 3. Work items, in dependency order

### W1 — expose the stage seam  (prerequisite for everything else)

`planning/core.py`. Split the apply functions so the value feeding the
hypothesis stage is observable. Mirrors R's `apply_plan_stages()`.

```python
@dataclass(frozen=True)
class Stages:
    pre: np.ndarray   # after aggregation, before the hypothesis
    post: np.ndarray  # final


def prediction_plan_apply_stages(plan, pred) -> Stages:
    est = _apply_align(pred, plan.align)
    if plan.agg is not None:
        est = np.asarray(
            [_nan_weighted_mean(est[g.idx], g.w) for g in plan.agg], dtype=float
        )
    pre = est
    if plan.hyp is not None:
        est = _as_float_array(plan.hyp.apply(est))
    if est.shape[0] != plan.n_out:
        raise RuntimeError(
            "marginaleffects internal error: prediction plan replay changed shape"
        )
    return Stages(pre=pre, post=est)


def prediction_plan_apply(plan, pred) -> np.ndarray:
    return prediction_plan_apply_stages(plan, pred).post
```

Same treatment for `comparison_plan_apply`. Keep the existing single-value
functions as thin wrappers so no caller changes.

**Also in W1:** add `agg: list[AggGroup] | None = None` to `ComparisonPlan` and
populate it from `get_by_plan()` in `comparisons.py`, and add
`uses_y: bool = False` to `CompGroup`. Without the first, `avg_comparisons(by=...)`
can never reach the analytic path; without the second, `need_y` stays
conservative and disqualifies groups that ignore the outcome.

`need_y` is deliberately conservative — it is true for every custom comparison
function, because such a function *might* accept `y`. Whether any actually does
is a per-group fact, and that is the question a Jacobian needs answered:

```python
def plan_groups_use_y(plan) -> bool:
    if not plan.need_y:
        return False
    if not plan.groups:
        return True
    return any(g.uses_y for g in plan.groups)
```

### W2 — link-function adapter hook

Small and independent of W1; can land first.

`classes/model.py`, on `ModelAbstract`:

```python
    # Adapters predicting on a response scale return `(linkinv, mu_eta)` here.
    # `mu_eta` is the derivative of the inverse link, which turns a link-scale
    # design matrix into a response-scale Jacobian. `None` keeps the analytic
    # path from claiming the model.
    def get_link_functions(self):
        return None
```

`statsmodels/model.py`, on `ModelStatsmodels`:

```python
    def get_link_functions(self):
        # Read the derivative off the statsmodels link object rather than
        # switching on the link name, so custom and unlisted links work too.
        inner_model = self.model.model
        if type(inner_model).__name__ != "GLM":
            return None
        link = getattr(getattr(inner_model, "family", None), "link", None)
        if link is None:
            return None
        linkinv = getattr(link, "inverse", None)
        mu_eta = getattr(link, "inverse_deriv", None)
        if not callable(linkinv) or not callable(mu_eta):
            return None
        return linkinv, mu_eta
```

Reading `inverse_deriv` off the link object is the point. The existing
`_resolve_family_link` in `autodiff/pipeline.py` enumerates family and link names
and raises on anything unlisted; the analytic path must not inherit that
limitation, because a custom link is perfectly differentiable.

Offsets and exposure disqualify the model, exactly as `get_autodiff_args()`
already checks. Reuse that check rather than duplicating it.

### W3 — exact gradient registry

New module, `inference/gradients.py`. Each entry is one closed-form definition
from `estimands.py`, differentiated by hand with respect to the `hi` and `lo`
prediction vectors, returning the two gradient vectors.

Closed forms carry no step size, no structural assumption, and no verification
burden: they are correct wherever they are finite, and where they are not the
caller's finiteness check rejects the group.

```python
def comparison_gradient_exact(fun_key, hi, lo, args):
    """Return (grad_hi, grad_lo) for a recorded built-in, or None.

    `a` is the averaging weight vector: 1/n, or w/sum(w) when weighted. The
    `*avg` forms of the nonlinear keys aggregate the predictions *before*
    applying the function, so their gradient is evaluated at the aggregated
    predictions, not averaged from the row-level ones.
    """
    n = len(hi)
    w = args.get("w")
    if w is None or np.all(np.isnan(w)):
        a = np.full(n, 1.0 / n)
    else:
        w = np.asarray(w, dtype=float)
        if w.shape[0] != n or not np.isfinite(w).all() or w.sum() == 0:
            return None
        a = w / w.sum()

    def wmean(x):
        return float(np.sum(a * x))

    eps = args.get("eps")
    if fun_key.startswith(("dydx", "dyex", "expdydx")):
        if eps is None or not np.isfinite(eps).all() or np.any(np.asarray(eps) == 0):
            return None

    x = args.get("x")

    if fun_key == "difference":
        return np.ones(n), -np.ones(n)
    if fun_key in ("differenceavg", "differenceavgwts"):
        return a, -a
    if fun_key in ("ratio", "lift"):
        return 1 / lo, -hi / lo**2
    if fun_key in ("ratioavg", "ratioavgwts", "liftavg", "liftavgwts"):
        # liftavg = wmean(hi - lo) / wmean(lo) = wmean(hi)/wmean(lo) - 1,
        # so its gradient is the gradient of ratioavg.
        mh, ml = wmean(hi), wmean(lo)
        return a / ml, -a * mh / ml**2
    if fun_key == "lnratio":
        return 1 / hi, -1 / lo
    if fun_key in ("lnratioavg", "lnratioavgwts"):
        mh, ml = wmean(hi), wmean(lo)
        return a / mh, -a / ml
    if fun_key == "lnor":
        return 1 / (hi * (1 - hi)), -1 / (lo * (1 - lo))
    if fun_key in ("lnoravg", "lnoravgwts"):
        mh, ml = wmean(hi), wmean(lo)
        return a / (mh * (1 - mh)), -a / (ml * (1 - ml))
    if fun_key == "dydx":
        return np.full(n, 1 / eps), np.full(n, -1 / eps)
    if fun_key in ("dydxavg", "dydxavgwts"):
        return a / eps, -a / eps
    if fun_key == "dyex":
        if x is None:
            return None
        return x / eps, -x / eps
    if fun_key in ("dyexavg", "dyexavgwts"):
        if x is None:
            return None
        return a * x / eps, -a * x / eps
    if fun_key == "expdydx":
        return np.exp(hi) / (np.exp(eps) * eps), -np.exp(lo) / (np.exp(eps) * eps)
    if fun_key in ("expdydxavg", "expdydxavgwts"):
        return a * np.exp(hi) / (np.exp(eps) * eps), -a * np.exp(lo) / (np.exp(eps) * eps)
    return None
```

Two notes on coverage.

**`dyex` belongs here; `eyex` and `eydx` do not.** `dyex` is
`(hi - lo) / eps * x` — it depends on the predictor, which is a constant with
respect to `beta`. `eyex` and `eydx` divide by the fitted response, so their
gradient needs a product rule over an extra prediction. Their groups are marked
`uses_y`, which disqualifies them upstream. Do not lump the three together.

**A custom comparison closure records `fun_key = None` and always reaches the
numeric fallback unless a future API supplies an explicit, trusted derivative.**
Probe agreement cannot prove that arbitrary code is linear or differentiable.

### W4 — response-scale chain rule in the comparison Jacobian

`inference/analytic.py`. Fold the inverse-link derivative into the stage
gradient *before* either touches a model matrix, because the product is a vector
and the matrix is not:

```python
w_hi = grad_hi if d_hi is None else grad_hi * d_hi[idx]
w_lo = grad_lo if d_lo is None else grad_lo * d_lo[idx]

if group.scalar:
    # A group that contracts to a single value never needs the
    # observation-level derivative: its row is a pair of weighted column
    # sums, which crossprod forms without allocating the n x p matrix the
    # contraction would immediately discard.
    value = weighted_columns(X_hi, idx, w_hi) + weighted_columns(X_lo, idx, w_lo)
else:
    value = X_hi[idx] * w_hi[:, None] + X_lo[idx] * w_lo[:, None]
```

with

```python
def weighted_columns(X, idx, w):
    return w @ X if idx is None else w @ X[idx]
```

`_linear_design()` gains a response-scale branch: when `get_link_functions()`
returns a pair and the requested type is the response scale, compute
`eta = X @ beta` once and derive both `pred = linkinv(eta)` and
`d = mu_eta(eta)` from it. Compute `eta` at most once; it is wanted for the
derivative and again to rebuild the predictions.

### W5 — aggregation as a sparse contraction

An aggregation is a linear map, `out = W.T @ M`. Written densely `W` is
`n x n_agg` and the product costs `n * n_agg * p`. But the plan records exactly
one `(source row, output row)` pair per source row, so `W` has only `n` nonzero
entries and the same product costs a single pass.

Collect entries from every group at once rather than aggregating group by group;
the latter subsets `M` once per group and so copies the whole matrix across the
loop, several times the cost of the arithmetic it feeds.

For the common case of a single output row — an unstratified average — skip the
sparse matrix entirely and take one weighted column sum.

For an aggregating *prediction* estimand, fold the row scaling into the
aggregation weights so the contraction reads straight off `X`, never forming the
observation-level product it would immediately reduce.

### W6 — the hypothesis as a composable stage

Two changes, and the first is where most of the recovered coverage comes from.

**Promote linear hypotheses to matrices.** `hypothesis_compile.py` currently
emits `kind="string"` and `kind="formula"` with an opaque `apply`. Prove
linearity by structural recursion on the parsed expression: a sum or difference
of scaled coefficient references is linear, and its contrast matrix comes from
the proof itself. Anything else — unknown symbols, products of two references,
calls — is not provably linear and keeps its current kind. Verify the compiled
`H` with one matrix-vector product against the estimates it claims to reproduce
before trusting it.

An exact matrix representation is faster than any probe, exact rather than
approximate, and needs no verification beyond that single product.

**Compose the rest by probing.** A hypothesis that is not linear, or whose
linearity could not be proved, is still only a map from a handful of estimates to
a handful of tested quantities. Differentiating it costs nothing next to a model
evaluation, so compose rather than discard:

```python
def analytic_hypothesis(J, hyp, estimate_pre=None):
    if hyp is None:
        return J
    if hyp.kind == "matrix" and hyp.H is not None:
        H = np.asarray(hyp.H, dtype=float)
        if H.shape[0] == J.shape[0] and np.isfinite(H).all():
            return H.T @ J
    if not callable(hyp.apply) or estimate_pre is None:
        return None
    if len(estimate_pre) != J.shape[0]:
        return None
    G = stage_jacobian_dense(hyp.apply, estimate_pre)
    if G is None or G.shape[1] != J.shape[0]:
        return None
    return G @ J
```

`estimate_pre` is the `pre` value from W1. This is precisely why W1 comes first.

`stage_jacobian_dense` is a central-difference probe, column by column, with the
step scaled to the value being perturbed and `eps ** (1/3)` because that balances
truncation against roundoff for a central difference:

```python
STAGE_PROBE_MAX_DIM = 500


def stage_probe_step(x):
    return np.maximum(np.abs(x), 1.0) * np.finfo(float).eps ** (1 / 3)


def stage_jacobian_dense(f, x, n_out=None):
    x = np.asarray(x, dtype=float)
    if x.size == 0 or not np.isfinite(x).all():
        return None
    # Probing costs one call per estimate and each call is itself linear in
    # the number of estimates: trivial at ordinary sizes, quadratic at extreme
    # ones. Very wide stages keep their previous path rather than risk being
    # slower than the fallback they replace.
    if x.size > STAGE_PROBE_MAX_DIM:
        return None
    base = _try(f, x)
    if base is None:
        return None
    n_out = base.size if n_out is None else n_out
    if base.size != n_out:
        return None
    step = stage_probe_step(x)
    out = np.empty((n_out, x.size), dtype=float)
    for j in range(x.size):
        hi, lo = x.copy(), x.copy()
        hi[j] += step[j]
        lo[j] -= step[j]
        fhi, flo = _try(f, hi), _try(f, lo)
        if fhi is None or flo is None or fhi.size != n_out or flo.size != n_out:
            return None
        out[:, j] = (fhi - flo) / (2 * step[j])
    return out if np.isfinite(out).all() else None
```

where `_try` evaluates `f`, returns `None` on any exception, and requires a
finite numeric result.

### W7 — guards

Every one of these is a fail-closed correctness guard, not a debugging
assertion. Returning `None` is always an acceptable outcome: it preserves the
existing autodiff and finite-difference paths.

1. **Replay agreement.** The recovered stage output must reproduce the reported
   estimates before the Jacobian is trusted. This rejects stale matrices,
   offsets, prediction arguments, and future semantic changes the static
   whitelist does not capture. Python's `plan_values_allclose` is already
   element-wise — keep it that way. A mean-relative check would let one badly
   wrong row hide among many correct ones.
2. **Finiteness of the output.** Add a whole-matrix check. This is a backstop
   rather than the sole guard, because IEEE propagation is a property of IEEE
   arithmetic and not of every BLAS kernel: implementations that skip zero
   multiplicands can turn `0 * inf` into `0` instead of `nan`, so an `inf` in a
   zero-weighted input row is not guaranteed to surface.
3. **Missing predictions disqualify the estimand.** The replay stages tolerate
   them (aggregation averages with nan-skipping), but the sparse Jacobian
   aggregation divides by full group counts. Those two conventions agree only
   when nothing is missing.
4. **Offsets disqualify the model.**
5. **Coefficient names must be unique and must match the design matrix
   columns**, which are reordered to the coefficient order when they differ.
6. **Reject statically before scanning.** A group whose function has no recorded
   closed form can only end in rejection, so reject it before the model matrices
   are aligned. Data-dependent rejections stay where the data is.

### W8 — custom comparison functions

R and Python both reject custom comparison closures from the analytic path.
Custom comparison functions fall back unless a future API lets them carry an
explicit, trusted derivative. Deterministic re-probing remains a heuristic and
is not used to claim analytic differentiation.

## 4. Invariants

- Any failure returns `None` and falls back. Never a partial Jacobian, never a
  mix of methods within one estimand.
- Closed forms are only ever applied to recorded built-in keys. Custom functions
  without a trusted derivative always fall back.
- `uses_y` groups never reach the analytic path.
- The point estimates are never changed by any of this. Only standard errors
  move, and where they move the analytic values are the accurate ones.

## 5. Test checklist

Mirror the R coverage. A test is only meaningful if it asserts *which path ran*,
not merely that the numbers look plausible — expose the equivalent of
`components(x, "jacobian_method")` if Python has no such accessor yet.

- every key in W3, unit-level and averaged, weighted and unweighted;
- both aggregation families: keys averaged from row-level gradients
  (`difference`, `dydx`, `expdydx`) versus keys evaluated at aggregated
  predictions (`ratioavg`, `lnratioavg`, `lnoravg`, `liftavg`);
- GLM response scale against a link whose name is not in any enumeration, to
  prove `inverse_deriv` is really being read off the object;
- `avg_comparisons(by=...)`, which only works once `ComparisonPlan.agg` exists;
- string and formula hypotheses, asserting they now reach the analytic path;
- a nonlinear hypothesis, asserting it composes rather than disabling;
- fallback cases: custom comparison closure, `eyex`, `eydx`, offsets, missing
  predictions;
- an accuracy anchor per family, pinned to a closed form computed outside the
  package rather than to the package's own numeric path.

On accuracy anchors: standard errors for newly covered estimands can move in
about the fifth significant digit relative to finite differences. The analytic
values are the correct ones — a logit average marginal effect matches the exact
form to `2e-10` where finite differences are off by `2e-5`; unit-level Poisson
slopes to `4e-9` against `3.3e-3`. Pin to the closed form, not to the old value.

One check falls out for free and is worth asserting directly, with no numerical
reference: `lnor` on a logit response scale must collapse exactly to
`X_hi - X_lo`, since log-odds are linear in the coefficients.

## 6. Non-goals

- The x-side derivative stays numerical. Only the coefficient-side gradient is
  closed-form, so the two-point `(hi, lo)` structure is untouched and formula
  terms like `poly()` and splines need no special handling — the analytic path
  only ever sees an already-built design matrix.
- No change to the autodiff path. It remains the second choice, between analytic
  and finite differences.
- `eyex` and `eydx` stay on the fallback.
