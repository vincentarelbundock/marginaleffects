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


#' @rdname get_predict
#' @export
get_predict.rq <- function(model,
                           newdata = insight::get_data(model),
                           type = NULL,
                           conf.level = NULL,
                           ...) {
    assert_dependency("quantreg")
    # type argument of the method is used to specify confidence interval type
    # TODO: add support for this in `insight`
    out <- quantreg::predict.rq(model,
                                newdata = newdata,
                                ...)
    out <- data.frame(rowid = seq_len(nrow(newdata)),
                      predicted = out)
    return(out)
}


#' @include sanity_model.R
#' @rdname sanity_model_specific
#' @keywords internal
sanity_model_specific.rqs <- function(model, ...) {
    stop("`marginaleffects` only supports `quantreg::rq` models with a single `tau` value.")
}
