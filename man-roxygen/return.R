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
#'
#' See `?print.marginaleffects` for printing options.
#'
#' The `data.frame`s produced by `marginaleffects` come with several attributes to hold the original model, data, and much other information that can be used for post-processing. These attributes are not considered part of the public API of the package. Their names and contents can change without warning or notice. Users should not rely on them.
#'
#' Depending on the model, this information can use up a substantial amount of memory. To avoid saving these attributes, users can set a global option. Note that setting this option may result in a loss of functionality, such as the ability to call `hypotheses()` to post-process estimates.
#'
#' `options(marginaleffects_lean = TRUE)`
#'
