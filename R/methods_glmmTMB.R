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



#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.glmmTMB <- function(model, ...) {
    out <- stats::vcov(model, full = TRUE)
    return(out)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.glmmTMB <- function(model, ...) {
    out <- model$fit$par
    names(out) <- colnames(stats::vcov(model, full = TRUE))
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
    out$fit$parfull[names(out$fit$parfull) != "b"] <- coefs
    out$fit$par <- stats::setNames(coefs, names(out$fit$par))
    return(out)
}



#' @rdname sanitize_model_specific
sanitize_model_specific.glmmTMB <- function(model, vcov = NULL, calling_function = "marginaleffects", ...) {
    if (isTRUE(vcov) || is.null(vcov)) {
        insight::format_error(
            "By default, standard errors for models of class `glmmTMB` are not calculated. For further details, see discussion at {https://github.com/glmmTMB/glmmTMB/issues/915}.",
            "Set `vcov = FALSE` or explicitly provide a variance-covariance-matrix for the `vcov` argument to calculate standard errors."
        )
    }
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
