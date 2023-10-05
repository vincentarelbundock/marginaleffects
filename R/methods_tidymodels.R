#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.model_fit <- function(model, newdata, type = NULL, ...) {
    out <- stats::predict(model, new_data = newdata, type = type)[[".pred"]]
    out <- data.frame(rowid = seq_along(out), estimate = out)
    return(out)
}


#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.model_fit <- function(model, ...) {
    return(FALSE)
}