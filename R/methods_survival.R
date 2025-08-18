#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.survreg <- function(model, coefs, ...) {
    # Reverse engineering insight::get_get_parameters.survreg(),
    # which uses summary.survreg()

    nvar0 <- length(model$coefficients)
    nvar <- nrow(model$var)
    if (nvar > nvar0) {
        model[["coefficients"]][] <- coefs[-nvar0]
        model[["scale"]][] <- exp(coefs[nvar0])
    } else {
        model$coefficients[] <- coefs
    }

    model
}

#' @rdname get_predict
#' @export
get_predict.coxph <- function(
    model,
    newdata = insight::get_data(model),
    type = "lp",
    ...) {
    out <- stats::predict(model, newdata = newdata, type = type, ...)
    out <- data.frame(estimate = out)
    out <- add_rowid(out, newdata)
    return(out)
}


#' @rdname sanitize_model_specific
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
