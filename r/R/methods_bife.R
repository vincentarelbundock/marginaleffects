#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.bife <- function(
    model,
    newdata = get_modeldata(model),
    type = "response",
    ...) {
    pred <- stats::predict(model, X_new = newdata, type = type, ...)
    predict_vector_to_dt(pred, model = model, newdata = newdata, type = type)
}
