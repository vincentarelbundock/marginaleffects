#' @rdname get_predict
#' @export
get_predict.fixest <- function(
    model,
    newdata = get_modeldata(model),
    type = "response",
    mfx = NULL,
    ...) {
    insight::check_if_installed("fixest")

    if (is.null(type)) {
        calling_function <- if (!is.null(mfx)) mfx@calling_function else "predictions"
        type <- sanitize_type(
            model = model,
            type = type,
            calling_function = calling_function
        )
    }

    dots <- list(...)

    # some predict methods raise warnings on unused arguments
    unused <- c(
        "normalize_dydx",
        "step_size",
        "numDeriv_method",
        "conf.int",
        "internal_call"
    )
    dots <- dots[setdiff(names(dots), unused)]

    # fixest is super slow when using do call because of some `deparse()` call
    # issue #531: we don't want to waste time computing intervals or risk having
    # them as leftover columns in contrast computations
    pred <- try(
        stats::predict(
            object = model,
            newdata = newdata,
            type = type
        ),
        silent = TRUE
    )

    if (inherits(pred, "try-error")) {
        return(pred)
    }

    out <- data.table(estimate = as.numeric(pred))
    out <- add_rowid(out, newdata)

    return(out)
}


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.fixest <- function(model, vcov = TRUE, calling_function = "predictions", ...) {
    # issue #1487 is only a problem for standard errors
    if (isFALSE(vcov)) {
        return(model)
    }
    if (!isTRUE(getOption("marginaleffects_safe", default = TRUE))) {
        return(model)
    }

    msg <- paste(
        "For this model type, `marginaleffects` cannot take into account the uncertainty in fixed-effects parameters.",
        "This is especially consequential for predictions and their uncertainty intervals.",
        "Set `vcov=FALSE` to compute estimates without standard errors."
    )

    if (!is.null(model[["fixef_vars"]])) {
        # issue #1487: fixed-effects always matter for predictions
        if (identical(calling_function, "predictions")) {
            stop_sprintf(msg)
        }

        # issue #1487: fixed-effects matter for slopes and contrasts, except for linear models
        if (!is.null(model$family)) {
            if (identical(calling_function, "hypotheses")) {
                if (isTRUE(getOption("marginaleffects_safe", default = TRUE))) {
                    warn_sprintf(msg)
                }
            } else {
                stop_sprintf(msg)
            }
        }
    }

    return(model)
}


#' @rdname get_model_matrix
#' @export
get_model_matrix.fixest <- function(model, newdata, mfx = NULL) {
    # Skip the build for models (IV, fenegbin, feglm with fixed effects, ...)
    # whose analytic path would reject the matrix anyway.
    if (!is.null(mfx) && is.null(get_prediction_jacobian_spec(model, type = mfx@type))) {
        return(NULL)
    }
    # Absorbed fixed effects (and varying slopes) are constants with respect to
    # the coefficients, so the RHS design matrix is the full derivative.
    X <- stats::model.matrix(model, data = newdata, type = "rhs", collin.rm = TRUE)
    # `i()` omits columns for levels absent from `newdata`. Those columns are
    # identically zero for these rows.
    beta_names <- names(stats::coef(model))
    absent <- setdiff(beta_names, colnames(X))
    if (length(absent) > 0L) {
        X <- cbind(X, matrix(0, nrow(X), length(absent), dimnames = list(NULL, absent)))
    }
    X[, beta_names, drop = FALSE]
}


#' @noRd
#' @export
get_prediction_jacobian_spec.fixest <- function(model, type, ...) {
    # IV predictions depend on the endogenous regressors, not the RHS matrix
    if (isTRUE(model[["is_iv"]])) {
        return(NULL)
    }
    method_type <- model[["method_type"]]
    if (identical(method_type, "feols")) {
        return(prediction_jacobian_spec_linear(model, "fixest", type, c("response", "link")))
    }
    # the composer recomputes eta as X %*% beta, which omits fixed effects
    if (identical(method_type, "feglm") && is.null(model[["fixef_vars"]])) {
        return(prediction_jacobian_spec_glm_family(model, "fixest", type, family = model[["family"]]))
    }
    NULL
}
