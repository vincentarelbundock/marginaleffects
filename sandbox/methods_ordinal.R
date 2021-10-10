#' @rdname get_predict
#' @export
get_predict.clm <- function(model,
                            newdata = insight::get_data(model),
                            type = "response",
                            group_name = NULL,
                            ...) {

    checkmate::assert_choice(type,
                             choices = c("response", "prob", "cum.prob", "linear.predictor"))

    if (type == "response") {
        type <- "prob"
    }

    pred <- stats::predict(model,
                           newdata = newdata,
                           type = type)$fit
    sanity_predict_numeric(pred = pred, model = model, newdata = newdata, type = type)

    # numDeriv expects a vector
    if (is.matrix(pred) && (!is.null(group_name) && group_name != "main_marginaleffect")) {
        pred <- pred[, group_name, drop = TRUE]
    }

    sanity_predict_vector(pred = pred, model = model, newdata = newdata, type = type)
    return(pred)
}


#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.clm <- function(model, ...) {
    resp <- insight::get_response(model)
    if (is.factor(resp)) {
        out <- levels(resp)
    } else {
        out <- unique(resp)
    }
    return(out)
}
