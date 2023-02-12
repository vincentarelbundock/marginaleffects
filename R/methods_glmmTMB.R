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
        ...)
    return(out)
}



#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.glmmTMB <- function(model, ...) {
    out <- insight::get_parameters(model, component = "all")
    out$Parameter <- ifelse(out$Component == "zero_inflated", paste0("zi~", out$Parameter), out$Parameter)
    out$Parameter <- ifelse(out$Component == "dispersion", paste0("d~", out$Parameter), out$Parameter)
    out <- stats::setNames(out$Estimate, out$Parameter)
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
    out <- model
    idx <- !names(out$fit$parfull) %in% c("theta", "b", "psi")
    if (length(coefs) == length(out$fit$parfull[idx])) {
        out$fit$parfull[idx] <- stats::setNames(coefs, names(out$fit$parfull)[idx])
    } else {
        insight::format_error("Unable to compute standard errors for this model.")
    }

    idx <- !names(out$fit$par) %in% c("theta", "b", "psi")
    if (length(coefs) == length(out$fit$par[idx])) {
        out$fit$par[idx] <- stats::setNames(coefs, names(out$fit$par)[idx])
    } else {
        insight::format_error("Unable to compute standard errors for this model.")
    }

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
    flag <- !isTRUE(checkmate::check_flag(vcov, null.ok = TRUE)) &&
        !isTRUE(checkmate::check_matrix(vcov)) &&
        !isTRUE(checkmate::check_function(vcov))
    if (flag) {
        msg <- sprintf("This value of the `vcov` argument is not supported for models of class `%s`. Please set `vcov` to `TRUE`, `FALSE`, `NULL`, or supply a variance-covariance matrix.", class(model)[1])
        stop(msg, call. = FALSE)
    }
    return(model)
}
