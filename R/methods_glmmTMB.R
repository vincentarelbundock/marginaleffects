#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.glmmTMB <- function(model,
                                newdata = insight::get_data(model),
                                type = "response",
                                ...) {

    if (inherits(vcov, "vcov.glmmTMB")) {
        vcov <- vcov[[1]]
    }

    out <- get_predict.default(
        model = model,
        newdata = newdata,
        type = type,
        allow.new.levels = TRUE, # otherwise we get errors in marginal_means()
        fast = FALSE,
        ...)
    return(out)
}



#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.glmmTMB <- function(model, full = FALSE, ...) {
    if (isTRUE(full)) {
        out <- tryCatch(
            solve(TMB::sdreport(m$obj, getJointPrecision = TRUE)$jointPrecision),
            error = function(e) stats::vcov(model)$cond
        )
    } else {
        out <- stats::vcov(model)$cond
    }
    return(out)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.glmmTMB <- function(model, full = FALSE, ...) {
    out <- model$fit$parfull
    if (!isTRUE(full)) {
        out <- out[names(out) == "beta"]
    }
    out <- out[names(out) %in% c("beta", "b")]
    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.glmmTMB <- function(model, coefs, ...) {
    # internally, coefficients are held in model$fit$parfull and in
    # model$fit$par. It looks like we need to manipulate both for the
    # predictions and delta method standard errors to be affected.
    # random parameters are ignored: named "b"
    # the order matters; I think we can rely on it, but this still feels like a HACK
    # In particular, this assumes that the order of presentation in coef() is always: beta -> betazi -> betad
    out <- model
    bbeta <- coefs[names(coefs) %in% c("beta", "b")]
    out$fit$parfull[names(out$fit$parfull) %in% names(bbeta)] <- bbeta
    out$obj$env$last.par[names(out$obj$env$last.par) %in% names(bbeta)] <- bbeta
    out$obj$env$last.par.best[names(out$obj$env$last.par.best) %in% names(bbeta)] <- bbeta
    beta <- coefs[names(coefs) %in% c("beta")]
    out$fit$par[names(out$fit$par) %in% names(beta)] <- beta
    out$sdr$par.fixed[names(out$sdr$par.fixed) %in% names(beta)] <- beta
    return(out)
}



#' @rdname sanitize_model_specific
sanitize_model_specific.glmmTMB <- function(model, vcov = NULL, calling_function = "marginaleffects", ...) {
    REML <- as.list(insight::get_call(model))[["REML"]]
    if (isTRUE(REML) && !identical(vcov, FALSE)) {
        msg <- insight::format_message("Uncertainty estimates cannot be computed for `glmmTMB` models with the `REML=TRUE` option. Either set `REML=FALSE` when fitting the model, or set `vcov=FALSE` when calling a `slopes` function to avoid this error.")
        stop(msg, call. = FALSE)
    }

    # we need an explicit check because predict.glmmTMB() generates other
    # warnings related to openMP, so our default warning-detection does not
    # work
    if (inherits(vcov, "vcov.glmmTMB")) {
        vcov <- vcov[[1]]
    }
    return(model)
}
