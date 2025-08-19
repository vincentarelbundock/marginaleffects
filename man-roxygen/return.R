#' @return A `data.frame` with one row per estimate. This data frame is pretty-printed by default, but users can interact with it as a regular data frame, with functions like `nrow()`, `head()`, `colnames()`, etc. Values can be extracted using standard `[,]` or `$` operators, and manipulated using external packages like `dplyr` or `data.table`.
#'
#' Columns may include:
#'
#' * `rowid`: row number of the `newdata` data frame
#' * `group`: (optional) value of the grouped outcome (e.g., categorical outcome models)
#' * `term`: the focal variable.
#' * `estimate`: an estimate of the prediction, counterfactual comparison, or slope.
#' * `std.error`: standard errors computed via the delta method.
#' * `p.value`: p value associated to the `estimate` column. The null is determined by the `hypothesis` argument (0 by default).
#' * `s.value`: Shannon information transforms of p values. See the S values vignette at [https://marginaleffects.com](https://marginaleffects.com) the marginaleffects website.
#' * `conf.low`: lower bound of the confidence (or credible) interval defined by the `conf_level` argument.
#' * `conf.high`: upper bound of the confidence (or credible) interval defined by the `conf_level` argument.
#' * `predicted_lo`: predicted outcome for the "low" value of the focal predictor in a counterfactual comparison.
#' * `predicted_hi`: predicted outcome for the "high" value of the focal predictor in a counterfactual comparison.
#' * `p.equivalence`: share of posterior draws in the interval specified by the `equivalence` argument. This is only available for Bayesian models.
#' * `rope`: share of the posterior draws between `conf.low` and `conf.high` that are covered by the interval specified by the `equivalence` argument.
#' * `statistic.noninf`: test statistic for non-inferiority test (when `equivalence` argument is used).
#' * `statistic.nonsup`: test statistic for non-superiority test (when `equivalence` argument is used).
#' * `p.value.noninf`: p-value for non-inferiority test (when `equivalence` argument is used).
#' * `p.value.nonsup`: p-value for non-superiority test (when `equivalence` argument is used).
#' * `p.value.equiv`: p-value for equivalence test using Two One-Sided Tests (TOST) approach (when `equivalence` argument is used).
#'
#' See `?print.marginaleffects` for printing options.
#'
#' The `data.frame`s produced by `marginaleffects` stores an attribute that holds many internal objects, such as the original model, data, and much other information that can be used for post-processing. This information can be inspected using the `components()` function.
#'
#' Warning: The internal attributes retrieved by `components()` are not considered part of the public API of the package. Their names and contents can change without warning or notice. Users should not rely on them.
#'
#' Warning: In some cases, the internal attributes used by `marginaleffects()` can use up a substantial amount of memory. To clear this data, use the `prune()` function or set `options(marginaleffects_lean=TRUE)`.
#'
