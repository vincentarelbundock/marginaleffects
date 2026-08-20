# SPEC: performance work outside the vcov / Jacobian / standard-error paths

Scope: the estimate pipeline only. Nothing here touches `get_vcov`, `get_jacobian*`,
`jacobian_stage`, `get_se_delta`, or `autodiff`.

## How this was measured

`Rprof(line.profiling = TRUE)` with `pkgload::load_all()`, passing `vcov = FALSE`
to isolate estimate-path work. Three regimes were profiled separately, because
they have completely different bottlenecks:

- **Large N** (20k–200k rows): dominated by allocation and GC. At N = 50k,
  `<GC>` alone was 41% of self time.
- **Fixed overhead** (mtcars-sized): dominated by `insight` metadata calls and S4
  construction. This is the regime most interactive use lives in.
- **Resampling inference** (`inferences(method = "boot")`, R = 300): the whole
  pipeline reruns R times, so every fixed cost is multiplied by R.

Medians of 15 runs with `gc()` before each. Single runs on this machine are noisy
at the 10–20% level.

Line numbers below match the code at profiling time. Match on the surrounding
code, not the line number.

## Part 1 — changes worth applying

Three mechanical changes. Each was measured in isolation; none changes any
reported number.

### 1. `pad.R:27` — level-presence check coerces the whole factor column to character

```r
if (!all(levels(newdata[[v]]) %in% newdata[[v]]))                 # before
if (!all(tabulate(newdata[[v]], nlevels(newdata[[v]])) > 0L))     # after
```

`%in%` on a factor dispatches to `as.character.factor`, allocating one string per
row, for every factor column, on every `pad()` call. `tabulate()` works on the
integer codes and allocates nothing.

Equivalent on the edges: `levels()` never contains `NA` (absent `addNA`) and
`tabulate()` skips `NA` codes, so both ignore `NA` identically; the zero-level
case yields no vault entry on both sides.

Measured: 0.93 ms -> 0.08 ms per column at N = 100k (11x). Roughly 6% of
`predictions()` runtime (`"%in%" 6#27 "pad"` was 14 of 235 samples).

### 2. `pad.R:69` (`unpad`) — full table copy even when nothing was padded

```r
idx <- out$rowid > 0
if (isTRUE(all(idx))) {
    return(list(out = out, draws = draws))
}
out <- out[idx, , drop = FALSE]
```

The subset ran unconditionally. The only caller, `predictions_plan.R:64`, already
computes `idx_keep <- out$rowid > 0` and tests `!all(idx_keep)` two lines before
calling `unpad()`, which recomputes the same predicate — a later cleanup could
pass the result in.

Measured: 0.31 ms -> 0.12 ms at N = 100k.

### 3. `get_modeldata.R:56` — unconditional deep copy

```r
if (data.table::is.data.table(modeldata)) {
    modeldata <- data.table::copy(modeldata)
}
data.table::setDF(modeldata)
```

The existing comment explains the copy guards against data.table reference
semantics — but it also deep-copies plain data.frames, which already have
copy-on-write.

Safe because `setDF()` does not mutate a plain data.frame by reference: dropping
the copy leaves it operating on a caller-owned frame, and row names and
attributes survive intact on the caller's object.

Measured: 0.118 ms -> 0.003 ms for a 100k x 5 frame.

## Part 2 — `tmp_idx` has two consumers

Not a performance item. Recorded because it is a live uncovered path, and because
it kills an optimization that looks obviously safe.

`comparisons_plan.R` computes `out[, tmp_idx := seq_len(.N), by = tmp]`, a
`forderv` sort over N x K rows, worth about 7% of `avg_comparisons` at N = 20k.
It is tempting to skip that sort when `context$elasticities` is empty, on the
theory that `tmp_idx` only ever indexes `context$elasticities[[term]]`.

**It does not.** `comparison_call_args` also uses it for row alignment:

```r
if ("newdata" %in% fun_formals) {
    args[["newdata"]] <- comparison_subset_newdata(context$newdata, tmp_idx)
}
```

This fires whenever a user-supplied `comparison` function declares a `newdata`
argument, independently of elasticities. Falling back to a global `seq_len(.N)`
replaces a within-group counter with one that runs to N x K, while
`context$newdata` holds only N rows — `comparison_subset_newdata` then hard-stops
on `any(idx > nrow(newdata))`. A two-variable `avg_comparisons()` with a
`newdata`-taking comparison function works today over 32 `mtcars` rows, where the
global counter would reach 64.

**No test covers this path** — nothing in the suite passes a `comparison` function
declaring `newdata`. A green suite is not evidence for any change touching
`tmp_idx`. That regression test is worth adding on its own merits.

The sort is skippable only when *neither* elasticities *nor* a `newdata`-taking
comparison function is in play. A guard testing both recovers less than 7%, since
it declines to skip in more cases.

## Part 3 — leads, measured but not implemented

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

## Part 4 — what caps all of this

At large N, after the Part 1 patches, `predict.lm` -> `model.frame` ->
`model.matrix` is about 30% of runtime and GC is about 40%, much of the latter
being churn from allocating the N x K `hi` / `lo` tables. Nothing in Parts 1 or 3
touches either. Large-N `avg_comparisons` will not go far past -20% however many
leads land.

The structural change that would move that number: reuse the model matrix on the
estimate path. `get_comparisons_data` builds it only when `vcov` is a matrix, so
`vcov = FALSE` falls back to `predict()` rebuilding the model frame from scratch,
separately for `hi` and for `lo`. Whether that wins depends on whether one
model-matrix build plus two matrix-vector products beats two full `predict()`
calls. **This has not been measured.** It is the one item here that could
plausibly be worth more than everything above combined, and it should be
benchmarked before any of the Part 3 leads are implemented.

## Part 5 — sequencing

Land this separately from unrelated in-flight work, so a post-merge regression is
never ambiguous about which change caused it.

1. Add the regression test from Part 2 — a `comparison` function declaring
   `newdata`, exercised across more than one term.
2. Apply the three Part 1 changes as one small branch off `main`, and measure the
   combined effect rather than assuming the isolated figures add up.
3. Measure the Part 4 model-matrix reuse before implementing any Part 3 lead. If
   it pays, some of this gets rewritten anyway. Cheap to measure, and it gates
   the rest.
