#' @rdname get_vcov
#' @export
get_vcov.geeglm <- function(model,
                            vcov = NULL,
                            ...) {

    if (!is.null(vcov) && !is.logical(vcov)) {
        stop("The `vcov` argument is not supported for models of this class.")
    }

    # insight::get_varcov returns `summary(model)$cov.unscaled` and we need the
    # default one to match emmeans.
    out <- stats::vcov(model)
    return(out)
}
