#' @rdname get_predict
#' @export
get_predict.mlogit <- function(model,
                               newdata,
                               ...) {

    mat <- stats::predict(model, newdata = newdata)
    out <- data.table(rowid = rep(seq_len(nrow(mat)), rep = ncol(mat)),
                      group = rep(colnames(mat), each = nrow(mat)),
                      predicted = as.vector(mat))
    setkey(out, rowid, group)
    if ("term" %in% colnames(newdata)) {
        out[, "term" := newdata[["term"]]]
    }
    return(out)
}
