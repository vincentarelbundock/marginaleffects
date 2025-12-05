#' @section comparison argument functions:
#'
#' The following transformations can be applied by supplying one of the shortcut strings to the
#' `comparison` argument.

#' `hi` is a vector of adjusted predictions for the "high" side of the
#' contrast. `lo` is a vector of adjusted predictions for the "low" side of the
#' contrast. `y` is a vector of adjusted predictions for the original data. `x`
#' is the predictor in the original data. `eps` is the step size to use to
#' compute derivatives and elasticities.
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
