#' @include get_predict.R
#' @rdname @get_predict
#' @keywords internal
#' @export
get_predict.bife <- function(model,
                             newdata = insight::get_data(model),
                             vcov = FALSE,
                             conf_level = 0.95,
                             type = "response",
                             ...) {

    type <- sanity_type(model, type)

    pred <- stats::predict(model,
                           X_new = newdata,
                           type = type)

    sanity_predict_vector(pred = pred, model = model, newdata = newdata, type = type)
    sanity_predict_numeric(pred = pred, model = model, newdata = newdata, type = type)

    out <- data.frame(
        rowid = 1:nrow(newdata),
        predicted = pred)

    return(out)
}

