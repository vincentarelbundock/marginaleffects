#' @rdname get_predict
#' @export
get_predict.tobit1 <- function(model,
                               newdata = insight::get_data(model),
                               vcov = NULL,
                               conf_level = 0.95,
                               type = "response",
                               ...) {

    out <- stats::predict(model, what = type, newdata = newdata)
    out <- data.frame(rowid = seq_len(length(out)), estimate = out)
    return(out)
}
