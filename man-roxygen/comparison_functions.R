#' @section Comparison functions:
#'
#' Each of the quantities computed by `comparisons()` can be defined as a function of these quantities:
#'
#' - `hi`: vector of predictions for the "high" side of the contrast. 
#' - `lo`: vector of predictions for the "low" side of the contrast. 
#' - `y`: predictions for the original data. 
#' - `x`: focal predictor in the original data.
#' - `w`: weights
#'
#' For example, the "lift" of a binary predictor is a popular quantity of
#' interest, defined as the difference between predictions when the focal
#' predictor \eqn{X = 1}, and predictions when the focal predictor is
#' \eqn{X = 0}, normalized by the starting point. Or:
#'
#' \eqn{\frac{\hat{Y}_{X=1} - \hat{Y}_{X=0}}{\hat{Y}_{X=0}}}
#'
#' When, the argument is set to `comparison="lift"`, `marginaleffects` will compute the quantity using this function:
#'
#' `function(hi, lo) { (hi - lo) / lo }`
#'
#' Users can supply custom functions to the `comparison` argument, or use one of the many shortcuts available for common quantities of interest:
#'
#' ```{r, echo = FALSE, results = "asis"}
#' k <- marginaleffects:::comparison_function_dict
#' k <- sapply(k, deparse1, collapse = "")
#' #k <- gsub("^function ", "\\\\\\\\", k)
#' k <- data.frame(
#'     Shortcut = names(k),
#'     Function = k)
#' k <- k[!grepl("wts$", k$Shortcut),]
#' knitr::kable(k, format = "pipe", row.names = FALSE)
#' ```
