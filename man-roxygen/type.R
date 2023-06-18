#' @section Prediction types:
#' 
#' The `type` argument determines the scale of the predictions used to compute quantities of interest with functions from the `marginaleffects` package. Admissible values for `type` depend on the model object. When users specify an incorrect value for `type`, `marginaleffects` will raise an informative error with a list of valid `type` values for the specific model object.
#' 
#' Some of the most common `type` values are:
#' 
#' 
#' ```{r, echo = FALSE, results = "asis"}
#' k <- marginaleffects:::type_dictionary$type
#' k <- unique(c("response", "link", sort(k)))
#' k <- paste(k, collapse = ", ")
#' cat(k)
#' ```
#' 
