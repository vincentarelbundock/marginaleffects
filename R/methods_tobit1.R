#' @rdname get_predict
#' @export
get_predict.tobit1 <- function(
    model,
    newdata = get_modeldata(model),
    type = "response",
    ...) {
    out <- stats::predict(model, what = type, newdata = newdata)
    out <- data.table(estimate = out)
    out <- add_rowid(out, newdata)
    return(out)
}
