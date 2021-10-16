#' @rdname get_predict
#' @export
get_predict.clm <- function(model,
                            newdata = insight::get_data(model),
                            type = "response",
                            group_name = NULL,
                            ...) {

    checkmate::assert_choice(type, choices = c("response", "prob"))

    if (type == "response") {
        type <- "prob"
    }

    # `predict.clm()` only makes predictions for the observed response group of
    # each observation in `newdata`. When we remove the response from
    # `newdata`, `predict.clm()` makes predictions for all levels, which is
    # what we want.
    resp <- insight::find_response(model)
    newdata <- newdata[, setdiff(colnames(newdata), resp), drop = FALSE]

    # Corner case: The `predict.clm` method does not make predictions when the
    # response was transformed to a factor in the formula AND the response is
    # missing from `newdata`.
    lhs <- names(attr(stats::terms(model), "dataClasses"))[1]
    if (isTRUE(grepl("^factor\\(", lhs))) {
        stop("The response variable should not be transformed to a factor in the formula. Please convert the variable to factor before fitting your model.")
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
get_group_names.clm <- get_group_names.polr
