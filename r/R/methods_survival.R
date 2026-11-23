#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.survreg <- function(model, coefs, ...) {
    # `coefs` holds the regression coefficients first, followed by one
    # `Log(scale)` entry per stratum (`insight::get_parameters.survreg()` builds
    # it from `summary.survreg()`). Peel the scale entries off the tail: indexing
    # by `nvar0` dropped a regression coefficient and read the scale from the
    # wrong slot.

    nvar0 <- length(model$coefficients)
    nvar <- nrow(model$var)
    if (nvar > nvar0) {
        model[["coefficients"]][] <- coefs[seq_len(nvar0)]
        model[["scale"]][] <- exp(coefs[(nvar0 + 1):nvar])
    } else {
        model$coefficients[] <- coefs
    }

    model
}

#' @rdname set_coef
#' @export
set_coef.coxph <- function(model, coefs, ...) {
    # `predict.coxph(type = "expected" or "survival")` estimates the baseline
    # hazard from `exp(model$linear.predictors)`, so the stored linear predictors
    # must move with the coefficients. Otherwise the Jacobian ignores how the
    # baseline hazard depends on the coefficients. They are stored as
    # `X %*% beta + offset - sum(beta * means)`, so only the change in `beta` is
    # needed. `residuals` also depend on the coefficients, but `predict.coxph()`
    # reads them only when `newdata` is missing, and `get_predict()` always
    # supplies it.
    old <- model[["coefficients"]]
    model <- set_coef.default(model, coefs)
    delta <- model[["coefficients"]] - old
    delta[is.na(delta)] <- 0

    if (is.null(model[["linear.predictors"]]) || all(delta == 0)) {
        return(model)
    }

    # `model.matrix()` needs the model frame, which `predict.coxph()` rebuilds
    # anyway for types that read the linear predictors. If it cannot be rebuilt,
    # those types fail inside `predict()` regardless, and "lp" and "risk" do not
    # need the update. Penalized terms (e.g., `pspline()`, `frailty()`) do not
    # map one column to one coefficient, so they are left alone.
    X <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
    if (is.null(X) || ncol(X) != length(delta)) {
        return(model)
    }

    model[["linear.predictors"]] <- model[["linear.predictors"]] +
        drop(X %*% delta) - sum(delta * model[["means"]])

    model
}

#' @rdname get_predict
#' @export
get_predict.coxph <- function(
    model,
    newdata = get_modeldata(model),
    type = "lp",
    ...) {
    out <- stats::predict(model, newdata = newdata, type = type, ...)
    out <- data.table(estimate = out)
    out <- add_rowid(out, newdata)
    return(out)
}


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.coxph <- function(model, vcov, ...) {
    insight::check_if_installed("survival")
    flag1 <- !isFALSE(vcov)
    flag2 <- !isTRUE(checkmate::check_choice(vcov, choices = c("rsample", "boot", "fwb")))
    flag3 <- isTRUE(getOption("marginaleffects_safe", default = TRUE))
    if (flag1 && flag2 && flag3) {
        msg <- 'The default delta method standard errors for `coxph` models only take into account uncertainty in the regression coefficients. Standard errors may be too small. Use the `inferences()` function or set `vcov` to "rsample", "boot"  or "fwb" to compute confidence intervals by bootstrapping. Set `vcov` to `FALSE` or `options(marginaleffects_safe=FALSE)` to silence this warning.'
        warning(msg, call. = FALSE)
    }
    return(model)
}
