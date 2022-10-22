#' @export
get_predict.logitr <- function(model,
                               newdata = insight::get_data(model),
                               type = "prob",
                               ...) {
    type <- unname(type)
    obsID <- insight::get_call(model)$obsID
    out <- stats::predict(model, newdata = newdata, type = type, obsID = obsID)
    out <- data.frame(
        rowid = seq_len(nrow(out)),
        predicted = out[, 2])
    return(out)
}
