#' @rdname get_predict
#' @export
get_predict.mlogit <- function(model, newdata, ...) {
    mat <- stats::predict(model, newdata = as.data.frame(newdata))
    if (isTRUE(checkmate::check_atomic_vector(mat))) {
        out <- data.table(rowid = seq_along(mat), group = names(mat), estimate = mat)
    } else {
        out <- data.table(
            rowid = rep(seq_len(nrow(mat)), rep = ncol(mat)),
            group = rep(colnames(mat), each = nrow(mat)),
            estimate = as.vector(mat)
        )
    }
    setkey(out, rowid, group)
    if ("term" %in% colnames(newdata)) {
        out[, "term" := newdata[["term"]]]
    }
    # do not convert to factor because DV is often "yes" or "no" while the "group" is outcome levels.
    # out$group <- group_to_factor(out$group, model)
    return(out)
}


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
sanitize_model_specific.mlogit <- function(model, calling_function = NULL, ...) {
    if (!is.null(calling_function) && !identical(calling_function, "predictions")) {
        msg <- "`predictions()` and `avg_predictions()` are supported for `mlogit` models, but the other `marginaleffects` functions are not. The reason is that the data structure for these models is one observation-choice per row, which complicates internal handling by `marginaleffects`."
        stop_sprintf(msg)
    }
    return(model)
}
