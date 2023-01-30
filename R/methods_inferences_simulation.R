#' @rdname get_vcov
#' @export
get_vcov.inferences_simulation <- function(model, ...) return(NULL)


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.inferences_simulation <- function(model, vcov = FALSE, ...) {
    tmp <- model
    class(tmp) <- setdiff(class(tmp), "inferences_simulation")
    B <- get_coef(tmp)
    # at this stage, the `vcov` has been pre-processed, so we get all the "HC3" shortcuts
    V <- get_vcov(tmp, vcov = vcov)
    simfun <- attr(model, "inferences_simulate")
    R <- attr(model, "inferences_R")
    attr(model, "inferences_coefmat") <- simfun(R = R, B = B, V = V)
    return(model)
}


#' @rdname get_predict
#' @export
get_predict.inferences_simulation <- function(model, newdata, vcov = FALSE, ...) {
    coefmat <- attr(model, "inferences_coefmat")
    # coefmat: BxM 
    checkmate::assert_matrix(coefmat)
    # remove the special class to avoid calling myself
    mod <- model
    class(mod) <- setdiff(class(mod), "inferences_simulation")
    FUN <- function(coefs) {
        mod_tmp <- set_coef(mod, coefs = coefs)
        get_predict(mod_tmp, newdata = newdata)$estimate
    }
    # should never compute SE via delta method for these models
    out <- get_predict(mod, newdata = newdata, vcov = FALSE, ...)
    attr(out, "posterior_draws") <- apply(coefmat, MARGIN = 1, FUN = FUN)
    return(out)
}