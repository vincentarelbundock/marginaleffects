#' @rdname get_predict
#' @export
get_predict.MCMCglmm <- function(
    model,
    newdata,
    type = "response",
    mfx = NULL,
    newparams = NULL,
    ndraws = 1000,
    se.fit = NULL,
    ...
) {
    ndraws_mod <- nrow(model$VCV)
    if (ndraws < ndraws_mod) {
        idx <- sample.int(ndraws_mod, ndraws)
    } else {
        idx <- seq_len(ndraws_mod)
    }
    # hack: predict() appears to require the response in `newdata`, but the value does not appear to maek a difference
    nd <- newdata
    dvname <- insight::find_response(model)
    nd[, dvname] <- 1000
    draws <- lapply(
        idx,
        function(i) stats::predict(model, newdata = nd, it = i, ...)
    )
    draws <- do.call("cbind", draws)
    out <- data.frame(
        rowid = seq_len(nrow(nd)),
        estimate = apply(draws, MARGIN = 1, FUN = stats::median)
    )
    attr(out, "posterior_draws") <- draws
    return(out)
}


#' @rdname get_vcov
#' @export
get_vcov.MCMCglmm <- function(model, vcov = NULL, ...) {
    if (!is.null(vcov) && !is.logical(vcov)) {
        warn_sprintf(
            "The `vcov` argument is not supported for models of this class."
        )
    }
    vcov <- sanitize_vcov(model, vcov)
    return(NULL)
}
