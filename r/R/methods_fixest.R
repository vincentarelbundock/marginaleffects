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
    if (!is.null(mfx)) {
        # Unlike lm/glm, fixest predictions do not reuse the matrix, so it only
        # feeds the analytic Jacobian, which these settings bypass.
        if (
            !isTRUE(getOption("marginaleffects_analytic_jacobian", default = TRUE)) ||
                !is.null(settings_get("jacobian_function"))
        ) {
            return(NULL)
        }
        # `invlink(link)` predictions are differentiated on the link scale
        type <- if (identical(mfx@type, "invlink(link)")) "link" else mfx@type
        if (is.null(get_prediction_jacobian_spec(model, type = type))) {
            return(NULL)
        }
    }
    # Absorbed fixed effects (and varying slopes) are constants with respect to
    # the coefficients, so the RHS design matrix is the full derivative.
    X <- stats::model.matrix(model, data = newdata, type = "rhs", collin.rm = TRUE)
    beta_names <- names(stats::coef(model))
    if (identical(colnames(X), beta_names)) {
        return(X)
    }
    # `i()` omits columns for levels absent from `newdata`; those are zero.
    # But for bare `i()` terms, model.matrix() also drops the lowest level
    # present in `newdata` as if it were the reference (fixest#687), so a
    # missing column is only zero-filled when its level is provably absent.
    # Otherwise, fall back to finite differences.
    absent <- setdiff(beta_names, colnames(X))
    if (!fixest_absent_levels(model, newdata, absent)) {
        return(NULL)
    }
    # Columns which are not coefficients (e.g., the `i(..., ref =)1`
    # placeholder when every row is at the reference level) are dropped.
    # One allocation; absent columns stay zero, the rest are filled in place.
    out <- matrix(0, nrow(X), length(beta_names), dimnames = list(NULL, beta_names))
    # Column by column, so no second n x p temporary is allocated.
    for (k in intersect(beta_names, colnames(X))) {
        out[, k] <- X[, k]
    }
    out
}


# TRUE when every coefficient in `absent` is an `i()` level that does not occur
# in `newdata`, so its model-matrix column is identically zero.
# TODO: once lrberge/fixest#687 is fixed and marginaleffects requires a fixest
# version with the fix, missing columns can be zero-filled without this check.
fixest_absent_levels <- function(model, newdata, absent) {
    for (info in model[["model_matrix_info"]]) {
        idx <- match(absent, info[["coef_names_full"]])
        if (all(is.na(idx))) {
            next
        }
        # factor-by-factor interactions do not map coefficients to one level
        if (isTRUE(info[["is_inter_fact"]]) || !info[["f_name"]] %in% colnames(newdata)) {
            return(FALSE)
        }
        levels_absent <- as.character(info[["items"]][idx[!is.na(idx)]])
        if (any(levels_absent %in% as.character(newdata[[info[["f_name"]]]]))) {
            return(FALSE)
        }
        absent <- absent[is.na(idx)]
    }
    # anything left is missing for a reason other than an absent `i()` level
    length(absent) == 0L
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
