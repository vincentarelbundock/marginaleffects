#' @rdname get_predict
#' @export
get_predict.tobit1 <- function(
    model,
    newdata = insight::get_data(model),
    type = "response",
    ...) {
    out <- stats::predict(model, what = type, newdata = newdata)
    out <- data.frame(estimate = out)
    out <- add_rowid(out, newdata)
    return(out)
}
