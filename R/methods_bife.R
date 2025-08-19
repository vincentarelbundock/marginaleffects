#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.bife <- function(
    model,
    newdata = insight::get_data(model),
    type = "response",
    ...) {
    pred <- stats::predict(model, X_new = newdata, type = type, ...)

    sanity_predict_vector(
        pred = pred,
        model = model,
        newdata = newdata,
        type = type
    )
    sanity_predict_numeric(
        pred = pred,
        model = model,
        newdata = newdata,
        type = type
    )

    out <- data.table(estimate = pred)
    out <- add_rowid(out, newdata)

    return(out)
}
