# SPEC: performance work outside the vcov / Jacobian / standard-error paths

Status: findings + validated patch set, not yet applied to the working tree.
Date: 2026-08-19
Scope: the estimate pipeline only. Nothing here touches `get_vcov`, `get_jacobian*`,
`jacobian_stage`, `get_se_delta`, or `autodiff`.

## How this was measured

All profiling used `Rprof(line.profiling = TRUE)` with `pkgload::load_all()`, on the
`jacobian-stage-composition` branch. Estimate-path work was isolated by passing
`vcov = FALSE`, so no SE machinery runs. Three regimes were profiled separately,
because they have completely different bottlenecks:

- **Large N** (20k–200k rows): dominated by allocation and GC. At N = 50k,
  `<GC>` alone was 41% of self time.
- **Fixed overhead** (mtcars-sized): dominated by `insight` metadata calls and S4
  construction. This is the regime most interactive use lives in.
- **Resampling inference** (`inferences(method = "boot")`, R = 300): the whole
  pipeline reruns R times, so every fixed cost is multiplied by R.

Benchmarks report the median of 15 runs with `gc()` before each. Timing on this
machine is noisy at the 10–20% level for single runs; only medians over many
repetitions are trustworthy.

## Part 1 — validated changes

Five changes, implemented and verified in a scratchpad copy of the package. All
five are mechanical; none changes any reported number.

**Verification performed:**
- 20 hand-written cases (contrasts, cross-contrasts, elasticities, `by=`,
  weights, hypotheses, factor padding, single-row `newdata`) produce
  bit-identical output before and after.
- 1433 tinytest assertions across 70 test files, 0 failures, identical per-file
  assertion counts on both sides. Excluded: `autodiff`, `brms`, `bart`, `pkg-*`
  (optional dependencies).

Line numbers below were accurate when the profiles were taken. The working tree
has moved since, so match on the surrounding code rather than the line number.

### 1. `pad.R:27` — level-presence check coerces the whole factor column to character

```r
if (!all(levels(newdata[[v]]) %in% newdata[[v]]))       # before
if (!all(tabulate(newdata[[v]], nlevels(newdata[[v]])) > 0L))   # after
```

`%in%` on a factor dispatches to `as.character.factor`, allocating one string per
row, for every factor column, on every `pad()` call. `tabulate()` works on the
integer codes and allocates nothing. Both ignore `NA` identically.

Measured: 0.93 ms -> 0.08 ms per column at N = 100k (11x). Roughly 6% of
`predictions()` runtime; confirmed against the profile stacks
(`"%in%" 6#27 "pad"` was 14 of 235 samples).

### 2. `pad.R:69` (`unpad`) — full table copy even when nothing was padded

```r
idx <- out$rowid > 0
if (isTRUE(all(idx))) {
    return(list(out = out, draws = draws))
}
out <- out[idx, , drop = FALSE]
```

The subset ran unconditionally. Note that the only caller,
`predictions_plan.R:64`, already computes `all(idx_keep)` two lines before
calling `unpad()` — the check is duplicated, and a later cleanup could pass the
result in rather than recomputing it.

Measured: 0.31 ms -> 0.12 ms at N = 100k.

### 3. `comparisons_plan_frequentist.R:55` — `out[perm]` materializes a table that is then discarded

For any `*avg` comparison, the permuted copy of the full N x K-row wide table is
built at line 55 and then reduced to one row per group at line 106. The loop in
between reads only six columns.

Permute those six vectors instead, and map row indices back through `perm` when
the table is finally materialized:

```r
perm_store <- if (identical(perm, seq_len(n_out))) NULL else perm
pick <- function(v) if (is.null(perm_store) || is.null(v)) v else v[perm]
v_term <- pick(out[["term"]])
v_hi <- pick(out[["predicted_hi"]])
v_lo <- pick(out[["predicted_lo"]])
v_y <- pick(out[["predicted"]])
v_wts <- pick(out[["marginaleffects_wts_internal"]])
v_tmp_idx <- pick(out[["tmp_idx"]])
# ... loop reads v_* instead of out_sorted$* ...
keep <- unlist(out_rows, use.names = FALSE)
if (isTRUE(any_scalar_aggregate)) {
    out <- out[if (is.null(perm_store)) keep else perm[keep]]
} else if (!is.null(perm_store)) {
    out <- out[perm]
}
```

Measured: peak memory for `avg_comparisons` at N = 50k drops from 508 MB to
397 MB.

**Semantic change worth reviewing.** In the non-aggregate, identity-`perm` case
`out` is no longer copied, so the following `out[, estimate := ...]` mutates the
input table in place. The caller reassigns `out <- built$out`, and the suite
passes, but this is the one change in the set that is not purely additive.

### 4. `comparisons_plan.R:320` — grouped `tmp_idx` scan computed but unused

```r
tmp <- if (length(elasticities) > 0) grep("^term$|^contrast|^group$", colnames(out), value = TRUE) else character()
```

`out[, tmp_idx := seq_len(.N), by = tmp]` is a `forderv` sort over N x K rows.
`tmp_idx` only ever indexes `context$elasticities[[term]]`, which is an empty
list unless a comparison is `eyex` / `eydx` / `dyex` (or a user function taking
an `x` argument). When empty, `list()[[term]][tmp_idx]` is `NULL` and the
argument is dropped, so the sort is pure waste.

Measured: about 7% of `avg_comparisons` at N = 20k (`forderv` was 6.7% of self
time).

### 5. `get_modeldata.R:56` — unconditional deep copy

```r
if (data.table::is.data.table(modeldata)) {
    modeldata <- data.table::copy(modeldata)
}
data.table::setDF(modeldata)
```

The existing comment explains the copy guards against data.table reference
semantics — but it also deep-copies plain data.frames, which already have
copy-on-write.

Measured: 0.118 ms -> 0.003 ms for a 100k x 5 frame.

### Combined measured effect

Median of 15 runs. glm and lm on 100k rows; `mtcars` for the small case.

| case | before | after | delta |
|---|---|---|---|
| `predictions` (lm, 100k) | 0.030 s | 0.022 s | -27% |
| `predictions` (vcov = TRUE) | 0.059 s | 0.045 s | -24% |
| `avg_predictions(by = "f")` | 0.030 s | 0.022 s | -27% |
| `comparisons` unit-level (glm, 100k) | 0.895 s | 0.692 s | -23% |
| `avg_slopes` (glm, 100k) | 0.685 s | 0.615 s | -10% |
| `avg_comparisons` (glm, 100k) | 0.667 s | 0.616 s | -8% |
| `avg_comparisons` (vcov = TRUE) | 0.830 s | 0.758 s | -9% |
| `avg_comparisons` (mtcars) | 0.023 s | 0.021 s | -9% |

## Part 2 — leads, measured but not implemented

### L1. Repeated `insight` metadata calls (largest remaining lead)

A single `avg_comparisons()` on `mtcars` makes 13 `find_formula`, 6 `get_call`,
4 `find_predictors`, 4 `find_response`, 2 `find_variables`, 2 `find_weights` and
72 `is_model` calls. Call sites cluster in `class.R:126/135/142`,
`detect_variable_class` (`sanitize_variables.R:521/566/620`) and `get_coef.R:19`.

Together these are roughly **27% of bootstrap runtime**, because
`method = "boot" / "fwb" / "rsample"` reruns the entire user-facing pipeline once
per resample.

**This lead has a design risk that decides its whole payoff.** A cache keyed on
the model object hits every iteration under `method = "simulation"` (one model
object, perturbed coefficients) and never under `boot` / `fwb` / `rsample`
(the model is refit, so every iteration presents a new object). Making it hit
there means keying on something stable across refits — the formula or terms —
and accepting that `detect_variable_class` must still rerun, since variable
classes depend on the resampled data. Decide this before implementing.

### L2. `hush()` opens a text connection on every call

`hush.R:4` wraps everything in `utils::capture.output`, which creates a
`textConnection` and sinks output. Measured 35 us versus 5 us for the same
suppression via `withCallingHandlers` alone, at roughly 12 calls per invocation.

Cheap, but it drops `print` / `cat` suppression — some model methods do print, so
this is a behavior change, not a pure optimization. `get_model_matrix.R:88`
already carries a comment about avoiding "the connection overhead of
`hush()` / `capture.output()`", so the cost is known.

### L3. `merge_original_data` copies the payload three times

`utils.R:149` builds `tmp <- original[, ..cols]`, `utils.R:162` then does
`cbind(out, tmp[, ..payload])` — a second subset and a third allocation. About 9%
of `predictions()` runtime; the profile shows `"[.data.table" 3#162` as 22 of 235
samples. Roughly half looks recoverable by selecting once.

Note: swapping `cbind` for `data.table::set()` was measured and is *slower* when
the target must be copied first. It only pays if the caller can guarantee `out`
is a fresh local table.

### L4. `predictions.R:335` — `data.table::data.table(tmp)`

Goes through `as.data.table.list` and `recycle`. `as.data.table()` is 2.8x faster
on a data.frame, `copy()` 1.4x on a data.table. Worth about 1% of `predictions()`;
listed only because it is a one-line change.

`as.data.table()` on an object that is already a data.table returns it without
copying, which would change reference semantics — check whether `tmp` is still
read after line 335 before making that specific substitution.

### L5. `get_comparisons_data` copies wide frames it does not need

- `get_comparisons_data.R:27` — `as.data.table(newdata)` copies every column.
  On a 200k x 42 frame where the model uses three variables, this was 7% of
  runtime. The existing comment explains why `setDT()` cannot be used
  (columns with more than one dimension, e.g. `idx` in mlogit).
- `get_comparisons_data.R:101` — `rbindlist()` copies again even when the list
  has a single element.

The single-element shortcut needs care: `original` aliases the `as.data.table()`
result, so today it is only `rbindlist`'s copy that stops the subsequent
`original[, "term" := ...]` from corrupting `newdata`.

## Part 3 — expected boost if everything lands

Validated column is measured. The leads column is an estimate with the stated
confidence, not a measurement.

| Workload | Validated 5 | + all leads | Confidence |
|---|---|---|---|
| `predictions` / `avg_predictions`, large N | -27% | **-32 to -38%** | high — the leads here are copy elimination already timed in isolation |
| `comparisons` / `slopes` unit-level, large N | -23% | **-27 to -32%** | medium |
| `avg_comparisons` / `avg_slopes`, large N | -8 to -10% | **-12 to -16%** | medium |
| Small model, interactive | -9% | **-25 to -30%** | medium-low — rides almost entirely on L1 |
| `inferences(method = "simulation")` | -9% | **-30 to -35%** | medium |
| `inferences(method = "boot" / "fwb" / "rsample")` | -9% | **-10% or -35%** | low — the L1 cache-key question decides which |

## Part 4 — what caps all of this

At large N, after the validated patches, `predict.lm` -> `model.frame` ->
`model.matrix` is about 30% of runtime and GC is about 40%, much of the latter
being churn from allocating the N x K `hi` / `lo` tables. Nothing in Parts 1 or 2
touches either. Large-N `avg_comparisons` will not go far past -20% however many
leads land.

The structural change that would move that number: reuse the model matrix on the
estimate path. `get_comparisons_data` builds it only when `vcov` is a matrix, so
`vcov = FALSE` falls back to `predict()` rebuilding the model frame from scratch,
separately for `hi` and for `lo`. Whether that wins depends on whether one
model-matrix build plus two matrix-vector products beats two full `predict()`
calls. **This has not been measured.** It is the one item here that could
plausibly be worth more than everything above combined, and it should be
benchmarked before any of the Part 2 leads are implemented.

## Reproducing

The validated patch set is applied to a scratchpad copy, re-appliable to a fresh
checkout with:

```
bash /tmp/claude-1000/-home-vincent-repos-marginaleffects/eada97da-a7f5-4409-b810-9fb0314b0585/scratchpad/apply_patches.sh <path-to-r-package>
```

That directory is session-scoped and will not survive indefinitely; the five
changes are small enough to reapply by hand from Part 1 if it is gone.
