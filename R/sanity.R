#' check if dependency is installed
#'
#' @noRd
check_dependency <- function(library_name) {
  flag <- requireNamespace(library_name, quietly = TRUE)
  if (!isTRUE(flag)) {
      msg <- sprintf("Please install the `%s` package.", library_name)
      return(msg)
  } else {
      return(TRUE)
  }
}
assert_dependency <- checkmate::makeAssertionFunction(check_dependency)


sanity_wts <- function(wts, newdata) {
    # weights must be available in the `comparisons()` function, NOT in
    # `tidy()`, because comparisons will often duplicate newdata for
    # multivariate outcomes and the like. We need to track which row matches
    # which.
    if (!is.null(wts)) {
        flag1 <- isTRUE(checkmate::check_string(wts)) && isTRUE(wts %in% colnames(newdata))
        flag2 <- isTRUE(checkmate::check_numeric(wts, len = nrow(newdata)))
        if (!flag1 && !flag2) {
            msg <- sprintf("The `wts` argument must be a numeric vector of length %s, or a string which matches a column name in `newdata`. If you did not supply a `newdata` explicitly, `marginaleffects` extracted it automatically from the model object, and the `wts` variable may not have been available. The easiest strategy is often to supply a data frame such as the original data to `newdata` explicitly, and to make sure that it includes an appropriate column of weights, identified by the `wts` argument.",
                           nrow(newdata))
            stop(msg, call. = FALSE)
        }
    }
}

sanity_predict_vector <- function(pred, model, newdata, type) {
    if (!isTRUE(checkmate::check_atomic_vector(pred)) &&
        !isTRUE(checkmate::check_array(pred, d = 1))) {
        msg <- sprintf(
'`predict(model, type = "%s")` was called on a model of class `%s`, but this command did not produce the expected outcome: A numeric vector of length %s. This can sometimes happen when users try compute a marginal effect for some models with grouped or multivariate outcome which are not supported yet by `marginaleffects` package. Please consult your modeling package documentation to learn what alternative `type` arguments are accepted by the `predict` method, or file a feature request on Github:  https://github.com/vincentarelbundock/marginaleffects/issues',
        type, class(model)[1], nrow(newdata))
        stop(msg, call. = FALSE)
    }
}


sanity_predict_numeric <- function(pred, model, newdata, type) {
    if (!isTRUE(checkmate::check_numeric(pred))) {
        msg <- sprintf(
'`predict(model, type = "%s")` was called on a model of class `%s`, but this command did not produce the expected outcome: A numeric vector of length %s. This can sometimes happen when users try compute a marginal effect for an outcome type which is unsupported, or which cannot be differentiated. Please consult your modeling package documentation to learn what alternative `type` arguments are accepted by the `predict` method.',
        type, class(model)[1], nrow(newdata))
        stop(msg, call. = FALSE)
    }
}

# OBSOLETE CHECKS KEPT FOR POSTERITY

# sanitize_return_data <- function() {
#     return_data <- getOption("marginaleffects_return_data", default = TRUE)
#     checkmate::assert_flag(return_data)
#     return(return_data)
# }


# sanitize_numDeriv_method <- function() {
#     numDeriv_method <- getOption("marginaleffects_numDeriv_method", default = "simple")
#     checkmate::assert_choice(numDeriv_method, choices = c("simple", "complex", "Richardson"))
#     return(numDeriv_method)
# }


