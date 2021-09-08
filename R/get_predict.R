#' @title Get a named vector of predictficients from a model object
#' @rdname get_predict
#' @export
get_predict <- function (model, newdata, prediction_type, ...) {
    UseMethod("get_predict", model)
}

get_predict.default <- function(model, newdata, prediction_type, ...) {
    pred <- stats::predict(model, 
                           newdata = newdata, 
                           type = prediction_type)
    return(pred)
}
