sanity_equivalence_p_adjust <- function(equivalence, p_adjust) {
    if (!is.null(equivalence) && !is.null(p_adjust)) {
        insight::format_error("The `equivalence` and `p_adjust` arguments cannot be used together.")
    }
    checkmate::assert_choice(p_adjust, choices = stats::p.adjust.methods, null.ok = TRUE)
}


sanity_df <- function(df, x) {
    checkmate::assert(
        checkmate::check_number(df, lower = 1),
        checkmate::check_numeric(df, len = nrow(x)))
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


