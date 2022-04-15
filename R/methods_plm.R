#' @include sanity_model.R
#' @rdname sanity_model_specific
#' @keywords internal
sanity_model_specific.plm <- function(model, ...) {
    if ("within" %in% model$args$model) {
        stop('The `plm::predict` function does not appear to support the `newdata` argument when `plm(model="within")`. Therefore, `marginaleffects` cannot support "within" models, even if it supports many other models produced by the `plm` package. You may want to try the `fixest` package instead.')
    }
}

#' @rdname get_vcov
#' @export
get_vcov.plm <- function(model,
                         vcov = NULL,
                         ...) {

    if (!is.null(vcov) && !is.logical(vcov)) {
        stop("The `vcov` argument is not supported for models of this class.")
    }

    # get_varcov produces a very different results for unknown reason
    # https://github.com/easystats/insight/issues/556
    out <- stats::vcov(model)
    return(out)
}
