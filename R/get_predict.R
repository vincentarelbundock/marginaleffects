#' Get predicted values from a model object (internal function)
#'
#' @return A vector of predicted values of length equal to the number of rows
#' in `newdata`. For models with multi-level outcomes (e.g., multinomial
#' logit), this function returns a matrix of predicted values with column names
#' equal to each of the levels/groups.
#' @rdname get_predict
#' @inheritParams marginaleffects
#' @keywords internal
#' @export
get_predict <- function (model, newdata, type, ...) {
    UseMethod("get_predict", model)
}


#' @rdname get_predict
#' @export
get_predict.default <- function(model,
                                newdata = insight::get_data(model),
                                type = "response",
                                ...) {
    pred <- stats::predict(model,
                           newdata = newdata,
                           type = type)

    # if (!isTRUE(checkmate::check_atomic_vector(pred)) || 
    #     !isTRUE(checkmate::check_array(pred, d = 1)) ||
    #     !isTRUE(checkmate::check_numeric(pred))) {
    #     pred <- suppressWarnings(insight::get_predicted(model, predict = NULL, type = type, ...))
    #     if (inherits(pred, "get_predicted")) {
    #         pred <- as.numeric(pred)
    #     }
    # }

    sanity_predict_numeric(pred = pred, model = model, newdata = newdata, type = type)
    sanity_predict_vector(pred = pred, model = model, newdata = newdata, type = type)
    return(pred)
}
