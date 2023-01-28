#' @rdname get_predict
#' @export
get_predict.simulation_inference <- function(x, newdata, vcov = FALSE, ...) {
    coefmat <- attr(x, "coefmat")
    # coefmat: BxM 
    checkmate::assert_matrix(coefmat)
    # remove the special class to avoid calling myself
    mod <- x
    class(mod) <- setdiff(class(mod), "simulation_inference")
    FUN <- function(coefs) {
        mod_tmp <- set_coef(mod, coefs = coefs)
        get_predict(mod_tmp, newdata = newdata)$estimate
    }
    # should never compute SE via delta method for these models
    out <- get_predict(mod, newdata = newdata, vcov = FALSE, ...)
    attr(out, "posterior_draws") <- apply(coefmat, MARGIN = 1, FUN = FUN)
    return(out)
}


#' @rdname get_vcov
#' @export
get_vcov.simulation_inference <- function(x, ...) return(NULL)


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.simulation_inference <- function(model, vcov = FALSE, ...) {
    tmp <- model
    class(tmp) <- setdiff(class(tmp), "simulation_inference")
    B <- get_coef(tmp)
    if (isFALSE(vcov)) {
        vcov <- TRUE
    }
    V <- get_vcov(tmp, vcov = vcov)
    attr(model, "coefmat") <- attr(model, "simulate")(iter = attr(model, "iter"), B = B, V = V)
    attr(model, "V") <- V
    attr(model, "B") <- B
    return(model)
}