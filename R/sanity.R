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


sanity_contrast_numeric <- function(contrast_numeric, assertion = TRUE) {
    flag <- isTRUE(checkmate::check_numeric(contrast_numeric, min.len = 1, max.len = 2)) ||
            isTRUE(checkmate::check_choice(contrast_numeric, choices = c("iqr", "minmax", "sd", "2sd", "dydx")))
    if (isTRUE(assertion) && !isTRUE(flag)) {
        stop('Contrasts for numeric variables can be a single numeric value, a numeric vector of length 2, or one of the following strings: "iqr", "minmax", "sd", "2sd", "dydx"', call. = FALSE)
    } else {
        return(flag)
    }
}


sanity_contrast_factor <- function(contrast_factor, assertion = TRUE) {
    flag <- checkmate::check_choice(
        contrast_factor,
        choices = c("reference", "sequential", "pairwise", "all"))
    if (isTRUE(assertion) && !isTRUE(flag)) {
        stop('Contrasts for factor or character variables can be: "reference", "sequential", "pairwise", or "all".', call. = FALSE)
    } else {
        return(flag)
    }
}


sanitize_interaction <- function(interaction, variables, model) {
    # interaction: flip NULL to TRUE if there are interactions in the formula and FALSE otherwise

    checkmate::assert_flag(interaction, null.ok = TRUE)

    if (isTRUE(interaction) && is.null(variables)) {
        msg <- "When `interaction=TRUE` you must use the `variables` argument to specify which variables should be interacted."
        stop(msg, call. = TRUE)
    }

    if (isTRUE(checkmate::check_flag(interaction))) {
        return(interaction)
    }

    inter <- try(insight::find_interactions(model, flatten = TRUE), silent = TRUE)
    if (!is.null(variables) && isTRUE(length(inter) > 0)) {
        return(TRUE)
    } else {
        return(FALSE)
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


