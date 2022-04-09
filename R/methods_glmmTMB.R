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
