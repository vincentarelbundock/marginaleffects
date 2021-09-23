#' @include get_vcov.R
#' @rdname @get_vcov
#' @export
get_vcov.rq <- function(model, ...) {
    out <- insight::get_varcov(model)
    if (is.null(row.names(out))) {
        termnames <- names(stats::coef(model))
        colnames(out) <- termnames
        row.names(out) <- termnames
    }
    return(out)
}


#' @include sanity_model.R
#' @rdname sanity_model_specific
#' @keywords internal
sanity_model_specific.rqs <- function(model, ...) {
    stop("`marginaleffects` only supports `quantreg::rq` models with a single `tau` value.")
}
