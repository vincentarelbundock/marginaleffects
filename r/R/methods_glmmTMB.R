#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.glmmTMB <- function(
    model,
    newdata = get_modeldata(model),
    type = "response",
    mfx = NULL,
    newparams = NULL,
    ...) {
    # hack to avoid re-optimization
    # see https://github.com/vincentarelbundock/marginaleffects/issues/1064
    b_vec <- model$obj$env$parList()$b
    if (length(b_vec) > 0) {
        model$modelInfo$map$b <- factor(rep(NA, length(b_vec)))
    }

    np <- model$fit$par
    if (!is.null(newparams)) {
        idx <- glmmTMB_coef_index(model)
        if (length(newparams) != length(idx)) {
            stop_sprintf("Unable to align the parameters of this `glmmTMB` model.")
        }
        np[idx] <- newparams
    }

    out <- get_predict.default(
        model = model,
        newdata = newdata,
        type = type,
        allow.new.levels = TRUE, # otherwise we get errors in marginal_means()
        newparams = np,
        ...
    )
    return(out)
}


#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.glmmTMB <- function(model, vcov, ...) {
    vcov <- sanitize_vcov(model, vcov)

    # glmmTMB's sandwich path can change the TMB environment state used by
    # conditional predictions. Restore it after extracting the covariance matrix.
    env <- model$obj$env
    if (is.environment(env)) {
        last_par <- if (exists("last.par", envir = env, inherits = FALSE)) env$last.par else NULL
        last_par_best <- if (exists("last.par.best", envir = env, inherits = FALSE)) env$last.par.best else NULL
        on.exit({
            if (!is.null(last_par)) env$last.par <- last_par
            if (!is.null(last_par_best)) env$last.par.best <- last_par_best
        }, add = TRUE)
    }

    out <- glmmTMB_vcov_full(model, sandwich = identical(vcov, "HC0"))
    idx <- glmmTMB_coef_index(model)
    out <- out[idx, idx, drop = FALSE]
    nam <- names(get_coef.glmmTMB(model))
    dimnames(out) <- list(nam, nam)
    return(out)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.glmmTMB <- function(model, ...) {
    # Coefficients are the estimated elements of `model$fit$par`, except the
    # random-effect variance parameters (`theta*`). This covers the
    # conditional, zero-inflation, and dispersion fixed effects, as well as
    # family parameters (`psi`) like ordinal cutpoints or the Tweedie power.
    # It excludes columns dropped for rank deficiency, mapped parameters, and
    # the fixed intercept of ordinal models, none of which are estimated.
    nam <- colnames(glmmTMB_vcov_full(model))
    idx <- glmmTMB_coef_index(model)
    out <- model$fit$par[idx]
    nam <- nam[idx]
    nam <- ifelse(names(out) == "beta", paste0("cond~", nam), nam)
    names(out) <- nam
    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.glmmTMB <- function(model, coefs, ...) {
    # use predict(`newparams`) for this kind of model
    return(model)
}

#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.glmmTMB <- function(model, vcov = TRUE, re.form, ...) {
    if (identical(vcov, "HC0")) {
        insight::check_if_installed("glmmTMB", minimum_version = "1.1.12")
    }

    # `NULL` is the default, used by `plot_*()`
    if (is.null(vcov)) vcov <- TRUE

    if (!isTRUE(checkmate::check_flag(vcov)) && !identical(vcov, "HC0")) {
        msg <- 'For this model type, `vcov` must be `TRUE`, `FALSE`, `"HC0"`, or `NULL`.'
        stop_sprintf(msg)
    }

    # REML integrates the fixed effects out of `model$fit$par`
    if (!isFALSE(vcov) && isTRUE(model$modelInfo$REML)) {
        msg <- "Standard errors are unavailable for `glmmTMB` models fitted with `REML = TRUE`. Set `vcov = FALSE` or refit with `REML = FALSE`."
        stop_sprintf(msg)
    }

    if (!isFALSE(vcov) && (missing(re.form) || (!isTRUE(is.na(re.form))))) {
        msg <- "For this model type, `marginaleffects` only takes into account the uncertainty in fixed-effect parameters. This is often appropriate when `re.form=NA`, but may be surprising to users who set `re.form=NULL` (default) or to some other value. Call `options(marginaleffects_safe = FALSE)` to silence this warning."
        warn_sprintf(msg)
    }

    return(model)
}


#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.glmmTMB <- function(model, ...) {
    if (glmmTMB_is_ordinal(model)) {
        return(get_group_names.polr(model))
    }
    return(get_group_names.default(model))
}


glmmTMB_is_ordinal <- function(model) {
    isTRUE(hush(stats::family(model)$family) == "ordinal")
}


# Covariance of the estimated parameters, labelled and aligned with `model$fit$par`.
glmmTMB_vcov_full <- function(model, sandwich = FALSE) {
    out <- stats::vcov(model, full = TRUE, include_nonest = FALSE, sandwich = sandwich)
    if (nrow(out) != length(model$fit$par)) {
        stop_sprintf("Unable to align the variance-covariance matrix with the parameters of this `glmmTMB` model.")
    }
    return(out)
}


# Positions in `model$fit$par` of the coefficients: all but random-effect variance parameters.
glmmTMB_coef_index <- function(model) {
    which(!startsWith(names(model$fit$par), "theta"))
}
