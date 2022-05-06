#' @rdname get_predict
#' @export
get_predict.mlogit <- function(model,
                               newdata,
                               ...) {

    mat <- stats::predict(model, newdata = newdata)
    if (isTRUE(checkmate::check_atomic_vector(mat))) {
        out <- data.table(rowid = seq_along(mat),
                          group = names(mat),
                          predicted = mat)
    } else {
    out <- data.table(rowid = rep(seq_len(nrow(mat)), rep = ncol(mat)),
                      group = rep(colnames(mat), each = nrow(mat)),
                      predicted = as.vector(mat))
    }
    setkey(out, rowid, group)
    if ("term" %in% colnames(newdata)) {
        out[, "term" := newdata[["term"]]]
    }
    return(out)
}


#' @include sanity_model.R
#' @rdname sanity_model_specific
#' @keywords internal
sanity_model_specific.mlogit <- function(model, newdata, ...) {
    if (!is.null(newdata)) {
        nchoices <- length(unique(model$model$idx[, 2]))
        if (!isTRUE(nrow(newdata) %% nchoices == 0)) {
            msg <- sprintf("The `newdata` argument for `mlogit` models must be a data frame with a number of rows equal to a multiple of the number of choices: %s.", nchoices)
            stop(msg, call. = FALSE)
        }
    }
}
