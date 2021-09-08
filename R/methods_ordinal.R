#' @rdname get_predict
#' @export
get_predict.clm <- function(model, 
                            newdata = insight::find_data(model), 
                            prediction_type = "probs", ...) {
    # notice: $fit
    pred <- stats::predict(model, 
                           newdata = newdata, 
                           type = prediction_type)$fit
    return(pred)
}
