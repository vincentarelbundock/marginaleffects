# Performance opportunities

## Native exact Jacobian extensions

- [ ] Evaluate a pure-matrix aggregation operator for `by = TRUE`, `by = FALSE`,
  and fixed weights. Do not add a coefficient-column replay loop.
- [ ] Evaluate matrix hypotheses only if they compose cleanly with the aggregation
  operator.
- [ ] Evaluate predictions and response-scale GLMs separately; the latter require
  multiplying each model-matrix row by `family(model)$mu.eta(eta)`.
- [ ] Benchmark any extension independently and retain the numerical fallback for
  nonlinear transformations and custom functions.

## General verification

- [ ] Add representative cases to `r/sandbox/benchmark_cran_vs_main.R` or a dedicated current-versus-fast-path benchmark.
- [ ] Record elapsed time and allocation changes separately.
- [ ] Run the targeted performance and simulation tests.
- [ ] Run the complete R test suite before merging any optimization.
