# Performance opportunities

## Native exact Jacobian for linear models

- [ ] Add a native exact-Jacobian path for supported linear estimands.
- Start with identity-link `lm` models, predictions, `comparison = "difference"`, linear aggregation, and matrix hypotheses.
- Reuse the model matrices already cached in replay plans:

  ```r
  J_prediction <- X
  J_difference <- X_hi - X_lo
  ```

- Extend plan aggregation so it can aggregate each column of a Jacobian matrix.
- Retain numerical differentiation for nonlinear transformations, unsupported hypotheses, and unsupported model classes.
- Relevant code: `r/R/plan_replay.R`, `r/R/get_se_delta.R`, and `r/R/get_jacobian.R`.
- Profile evidence from a 50,000-row `lm` comparison:
  - Jacobian calculation consumed 80.8% of total runtime.
  - Repeated matrix multiplication consumed 53.5%.
  - Total runtime was approximately 1.72 seconds.
- [ ] Benchmark the native path against finite differences across row counts and coefficient counts.
- [ ] Test numerical equivalence of estimates, Jacobians, standard errors, and confidence intervals.

## Vectorized built-in frequentist comparisons

- [ ] Add a vectorized fast path to `comparison_plan_apply()` for built-in comparison functions.
- Begin with `difference`, then consider `ratio`, `lnratio`, `lift`, and their averaging variants.
- Reuse the optimized operation-switch approach already used by the Bayesian comparison implementation.
- Avoid constructing argument lists and calling `do_call()` separately for every group when the operation can be applied to complete vectors.

  ```r
  if (all_builtin_difference) {
      est <- hi - lo
  } else {
      # Existing generic group loop for custom or irregular comparisons.
  }
  ```

- Preserve the generic group loop for custom functions, scalar-returning functions, weights, elasticities, and heterogeneous operations.
- Relevant code: `r/R/comparisons_plan_frequentist.R` and `r/R/comparisons_plan_bayesian.R`.
- Profile evidence: `comparison_plan_apply()` consumed 16.9% of a 50,000-row delta-method comparison call.
- [ ] Benchmark ordinary comparisons, delta-method inference, and simulation replay.
- [ ] Test all supported built-in operations against the generic implementation.

## Direct single-use prediction path for `lm`

- [ ] Add a guarded direct prediction path when an `lm` model has no cached model-matrix attribute.
- Restrict the initial path to full-rank, offset-free models and supported prediction types.
- Build the model matrix and multiply coefficients directly, while preserving pivot handling and rank-deficiency safeguards already used by `get_predict.lm()`.
- Continue using `predict.lm()` for offsets, unsupported types, unusual terms, and other uncertain configurations.
- Relevant code: `r/R/methods_aaa.R` and `r/R/get_model_matrix.R`.
- Benchmark evidence: direct model-matrix prediction was approximately 1.55x faster than `predict.lm()` on 100,000 rows.
- Profile evidence: prediction accounted for approximately 42% of a large `vcov = FALSE` comparison call.
- [ ] Benchmark predictions and comparisons with numeric, factor, and interaction terms.
- [ ] Test full-rank, rank-deficient, offset, weighted, and missing-data models against `predict.lm()`.

## General verification

- [ ] Add representative cases to `r/sandbox/benchmark_cran_vs_main.R` or a dedicated current-versus-fast-path benchmark.
- [ ] Record elapsed time and allocation changes separately.
- [ ] Run the targeted performance and simulation tests.
- [ ] Run the complete R test suite before merging any optimization.
