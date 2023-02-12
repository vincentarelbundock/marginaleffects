#' @rdname get_predict
#' @export
get_predict.rq <- function(model,
                           newdata = insight::get_data(model),
                           type = NULL,
                           ...) {

    # type argument of the method is used to specify confidence interval type
    # TODO: add support for this in `insight`
    insight::check_if_installed("quantreg")
    out <- quantreg::predict.rq(model,
                                newdata = newdata,
                                ...)
    out <- data.frame(rowid = seq_len(nrow(newdata)),
                      estimate = out)
    return(out)
}


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
sanitize_model_specific.rqs <- function(model, ...) {
    stop("`marginaleffects` only supports `quantreg::rq` models with a single `tau` value.", call. = FALSE)
}
