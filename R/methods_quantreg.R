#' @rdname get_predict
#' @export
get_predict.rq <- function(model,
                           newdata = insight::get_data(model),
                           vcov = NULL,
                           conf_level = 0.95,
                           type = NULL,
                           ...) {

    # type argument of the method is used to specify confidence interval type
    # TODO: add support for this in `insight`
    assert_dependency("quantreg") # predict method must be available
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
    stop("`marginaleffects` only supports `quantreg::rq` models with a single `tau` value.", call. = FALSE)
}
