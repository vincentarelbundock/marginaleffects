#' @section Prediction types:
#' 
#' The `type` argument determines the scale of the predictions used to compute quantities of interest with functions from the `marginaleffects` package. Admissible values for `type` depend on the model object. When users specify an incorrect value for `type`, `marginaleffects` will raise an informative error with a list of valid `type` values for the specific model object. The first entry in the list in that error message is the default type.
#' 
#' The `invlink(link)` is a special type defined by `marginaleffects`. It is available for some (but not all) models, and only for the `predictions()` function. With this link type, we first compute predictions on the link scale, then we use the inverse link function to backtransform the predictions to the response scale. This is useful for models with non-linear link functions as it can ensure that confidence intervals stay within desirable bounds, ex: 0 to 1 for a logit model. Note that an average of estimates with `type="invlink(link)"` will not always be equivalent to the average of estimates with `type="response"`. This type is default when calling `predictions()`. It is available---but not default---when calling `avg_predictions()` or `predictions()` with the `by` argument.
#' 
#' Some of the most common `type` values are:
#' 
#' ```{r, echo = FALSE, results = "asis"}
#' library(tinytable)
#' ty <- type_dictionary_build()
#' ty <- aggregate(type ~ class, data = ty, FUN = function(x) paste(x, collapse = ", "))
#' ty <- ty[order(ty$class), ]
#' print(tt(ty) |> theme_markdown(style = "gfm"), "markdown")
#' ```
#'
