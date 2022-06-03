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

    get_predict.default(
        model = model,
        newdata = newdata,
        vcov = vcov,
        conf_level = conf_level,
        type = type,
        allow.new.levels = TRUE, # otherwise we get errors in marginalmeans()
        ...)
}



#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.glmmTMB <- function(model, coefs) {
    # internally, coefficients are held in model$fit$parfull and in
    # model$fit$par. It looks like we need to manipulate both for the
    # predictions and delta method standard errors to be affected. In both
    # vectors, the parameters are named "beta***", so we only replace those,
    # and check if the length of the vector is OK.
    idx <- grepl("beta", names(model$fit$parfull))
    if (sum(idx) != length(coefs)) {
        msg <- "Could not manipulate the coefficients of this `glmmTMB` model object. Please report this error on the `marginaleffects` Github issue tracker with the data and code necessary to replicate the problem: https://github.com/vincentarelbundock/marginaleffects/issues"
        stop(msg, call. = FALSE)
    }
    model$fit$parfull[idx] <- coefs

    idx <- grepl("beta", names(model$fit$par))
    if (sum(idx) != length(coefs)) {
        msg <- "Could not manipulate the coefficients of this `glmmTMB` model object. Please report this error on the `marginaleffects` Github issue tracker with the data and code necessary to replicate the problem: https://github.com/vincentarelbundock/marginaleffects/issues"
        stop(msg, call. = FALSE)
    }
    model$fit$par[idx] <- coefs

    return(model)
}



#' @rdname sanity_model_specific
sanity_model_specific.glmmTMB <- function(model, vcov = NULL, ...) {
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
