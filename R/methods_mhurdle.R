#' @rdname get_predict
#' @export
get_predict.mhurdle <- function(
    model,
    newdata = insight::get_data(model),
    type = "response",
    ...
) {
    out <- stats::predict(model, what = type, newdata = newdata)
    out <- data.frame(rowid = seq_len(length(out)), estimate = out)
    return(out)
}


#' @rdname get_vcov
#' @export
get_vcov.mhurdle <- function(model, vcov = NULL, ...) {
    if (!is.null(vcov) && !is.logical(vcov)) {
        stop_sprintf(
            "The `vcov` for this class of models must be TRUE or FALSE."
        )
    }
    vcov <- sanitize_vcov(model, vcov)
    out <- try(stats::vcov(model), silent = TRUE)
    if (inherits(out, "try-error")) {
        out <- tryCatch(model[["vcov"]], error = function(e) NULL)
    }
    return(out)
}
