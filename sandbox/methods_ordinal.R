#' @rdname get_predict
#' @export
get_predict.clm <- function(model, 
                            newdata = insight::get_data(model), 
                            type = "prob", 
                            ...) {
    # notice: $fit
    pred <- stats::predict(model, 
                           newdata = newdata, 
                           type = type)$fit

    # numDeriv expects a vector
    if (is.matrix(pred) && (!is.null(group_name) && group_name != "main_marginaleffect")) {
        pred <- pred[, group_name, drop = TRUE]
    }

    sanity_predict_numeric(pred = pred, model = model, newdata = newdata, type = type)
    sanity_predict_vector(pred = pred, model = model, newdata = newdata, type = type)

    return(pred)
}


#' @include methods_nnet.R
#' @rdname get_group_names
#' @export
get_group_names.clm <- get_group_names.multinom
