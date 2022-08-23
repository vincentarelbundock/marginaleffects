#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.glmmTMB <- function(model,
                                newdata = insight::get_data(model),
                                vcov = FALSE,
                                conf_level = 0.95,
                                type = "response",
                                ...) {

    if (inherits(vcov, "vcov.glmmTMB")) {
        vcov <- vcov[[1]]
    }

    out <- get_predict.default(
        model = model,
        newdata = newdata,
        vcov = vcov,
        conf_level = conf_level,
        type = type,
        allow.new.levels = TRUE, # otherwise we get errors in marginalmeans()
        ...)
    return(out)
}



#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.glmmTMB <- function(model, ...) {
    # order matters
    b <- model$obj$env$parList(model$fit$par, model$fit$parfull)
    b[["b"]] <- NULL # no random effects
    b <- unlist(b)
    names(b) <- gsub("^beta\\d+", "beta", names(b))
    return(b)
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

    new <- make.unique(names(coefs))
    old <- make.unique(names(model$fit$parfull))
    idx <- match(old, new)
    out$fit$parfull <- stats::setNames(
        ifelse(is.na(idx), model$fit$parfull, coefs[idx]),
        names(model$fit$parfull))

    old <- make.unique(names(model$fit$par))
    idx <- match(old, new)
    out$fit$par <- stats::setNames(
        ifelse(is.na(idx), model$fit$par, coefs[idx]),
        names(model$fit$par))

    return(out)
}



#' @rdname sanity_model_specific
sanity_model_specific.glmmTMB <- function(model, vcov = NULL, calling_function = "marginaleffects", ...) {
    # insight handles predictions correctly
    if (!identical(calling_function, "predictions")) {
        REML <- as.list(insight::get_call(model))[["REML"]]
        if (isTRUE(REML) && !identical(vcov, FALSE)) {
            msg <- insight::format_message("Uncertainty estimates cannot be computed for `glmmTMB` models with the `REML=TRUE` option. Either set `REML=FALSE` when fitting the model, or set `vcov=FALSE` when calling a `marginaleffects` function to avoid this error.")
            stop(msg, call. = FALSE) 
        }
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
}
