#' @section Transformations:
#' 
#' The following transformations can be applied by supplying one of the shortcut strings to the
#' `transform_pre` argument. 

#' `hi` is a vector of adjusted predictions for the "high" side of the
#' contrast. `lo` is a vector of adjusted predictions for the "low" side of the
#' contrast. `y` is a vector of adjusted predictions for the original data. `x`
#' is the predictor in the original data. `eps` is the step size to use to
#' compute derivatives and elasticities.
#' 
#' ```{r, echo = FALSE}
#' k <- marginaleffects:::transform_pre_function_dict
#' k <- sapply(k, deparse1, collapse = "")
#' k <- gsub("^function ", "\\\\", k)
#' k <- data.frame(
#'     Shortcut = names(k),
#'     Function = k)
#' knitr::kable(k, row.names = FALSE)
#' ```
