#' @include get_predict.R
#' @rdname @get_predict
#' @keywords internal
#' @export
get_predict.bife <- function(model, 
                             newdata = insight::get_data(model), 
                             type = "response", 
                             ...) {

    pred <- stats::predict(model, 
                           X_new = newdata, 
                           type = type)

    sanity_predict_vector(pred = pred, model = model, newdata = newdata, type = type)
    sanity_predict_numeric(pred = pred, model = model, newdata = newdata, type = type)

    return(pred)
}


#' @include get_vcov.R
#' @rdname @get_vcov
#' @keywords internal
#' @export
get_vcov.bife <- function(model, ...) {
    beta <- get_coef(model)
    out <- stats::vcov(model)
    colnames(out) <- row.names(out) <- names(beta)
    return(out)
}
