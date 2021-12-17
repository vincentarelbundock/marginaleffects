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
get_predict <- function(model, newdata, type, ...) {
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

    # atomic vector
    if (isTRUE(checkmate::check_atomic_vector(pred))) {
        out <- data.frame(
            rowid = 1:nrow(newdata),
            # strip weird attributes added by some methods (e.g., predict.svyglm)
            predicted = as.numeric(pred))

    # matrix with outcome levels as columns
    } else if (is.matrix(pred)) {
        out <- data.frame(
            rowid = rep(1:nrow(pred), times = ncol(pred)),
            group = rep(colnames(pred), each = nrow(pred)),
            predicted = c(pred))

    } else {
        stop(sprintf("Unable to extractpreditions of type %s from a model of class %s. Please report this problem, along with reproducible code and data on Github: https://github.com/vincentarelbundock/marginaleffects/issues", type, class(model)[1]))
    }

    return(out)
}
