#' @export
get_predict.logitr <- function(model, newdata = insight::get_data(model), type = "prob", ...) {
    type <- unname(type)
    obsID <- insight::get_call(model)$obsID
    out <- stats::predict(model, newdata = newdata, type = type, obsID = obsID, returnData = FALSE)
    out <- data.frame(
        rowid = seq_len(nrow(out)),
        predicted = out[, 2]
    )
    return(out)
}


#' @export
sanitize_model_specific.logitr <- function(model, ...) {
    if (utils::packageVersion("insight") < "0.18.5.1") {
        msg <- c(
            'Support for `logitr` models requires a version of the `insight` package greater than 0.18.5. You can install the development version of `insight` with:',
            '',
            'install.packages("insight", repos = "https://easystats.r-universe.dev")'
        )
        stop_sprintf(msg)
    }
    return(invisible(NULL))
}
