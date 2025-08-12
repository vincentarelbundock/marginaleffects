#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.Learner <- function(model, newdata, type = NULL, ...) {
    if (!is.null(type) && !type %in% model$predict_types) {
        msg <- sprintf("Valid `type` values: %s", toString(model$predict_types))
        stop_sprintf(msg)
    }
    out <- drop(stats::predict(model, newdata = newdata, predict_type = type))
    out <- data.frame(rowid = seq_along(out), estimate = out)
    return(out)
}


#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.Learner <- function(model, ...) {
    return(FALSE)
}
