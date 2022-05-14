#' @include get_predict.R
#' @rdname @get_predict
#' @keywords internal
#' @export
get_predict.gamlss <- function(model,
                               newdata = insight::get_data(model),
                               vcov = FALSE,
                               conf_level = 0.95,
                               type = "response",
                               ...) {

    if (!isTRUE(checkmate::check_flag(vcov, null.ok = TRUE))) {
        msg <- "The `vcov` argument is not supported for models of this class."
        stop(msg, call. = FALSE)
    }

    # predict.gamlss() breaks when `newdata` includes unknown variables
    origindata <- insight::get_data(model)
    originvars <- colnames(origindata)
    tmp <- newdata[, originvars]
    out <- stats::predict(model, newdata = tmp, type = type, ...)

    if ("rowid" %in% colnames(newdata)) {
        out <- data.frame(rowid = newdata$rowid, predicted = out)
    } else {
        out <- data.frame(rowid = seq_along(out), predicted = out)
    }

    return(out)
}

