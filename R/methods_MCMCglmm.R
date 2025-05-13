#' @rdname get_predict
#' @export
get_predict.MCMCglmm <- function(
    model,
    newdata,
    type = "response",
    ndraws = 1000,
    ...) {

    ndraws_mod <- nrow(model$VCV)
    if (ndraws < ndraws_mod) {
        idx <- sample.int(ndraws_mod, ndraws)
    } else {
        idx <- seq_len(ndraws_mod)
    }
    draws <- lapply(idx, function(i) stats::predict(model, newdata = newdata, it = i, ...))
    draws <- do.call("cbind", draws)
    out <- data.frame(
        rowid = seq_len(nrow(newdata)),
        estimate = apply(draws, MARGIN = 1, FUN = stats::median))
    attr(out, "posterior_draws") <- draws
    return(out)
}


#' @rdname get_vcov
#' @export
get_vcov.MCMCglmm <- function(model,
                              vcov = NULL,
                              ...) {
    if (!is.null(vcov) && !is.logical(vcov)) {
        insight::format_warning("The `vcov` argument is not supported for models of this class.")
    }  
    vcov <- sanitize_vcov(model, vcov)
    return(NULL)
}
