#' @rdname get_predict
#' @export
get_predict.mlogit <- function(x, newdata, ...) {
    out <- stats::predict(x, newdata = newdata)
    out <- as.vector(out)
    out <- data.frame(rowid = seq_len(length(out)),
                      predicted = out)
    return(out)
}
