#' @rdname get_predict
#' @export
get_predict.clm <- function(model, 
                            newdata = insight::find_data(model), 
                            predict_type = "probs", ...) {
    # notice: $fit
    pred <- stats::predict(model, 
                           newdata = newdata, 
                           type = predict_type)$fit
    return(pred)
}
