#' @rdname get_predict
#' @export
get_predict.clm <- function(model, newdata, prediction_type, ...) {
    # notice: $fit
    pred <- stats::predict(model, 
                           newdata = newdata, 
                           type = prediction_type)$fit
    return(pred)
}
