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
                                conf.level = NULL,
                                ...) {

    # fast prediction for numeric differentiation
    if (is.null(conf.level)) {
        pred <- stats::predict(model,
                               newdata = newdata,
                               type = type)

        # 1-d array to vector (e.g., mgcv)
        if (is.array(pred) && length(dim(pred)) == 1) {
            pred <- as.vector(pred)
        }

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

    # slow prediction with `insight::get_predicted` for `predictions`.
    # this gives us nice back-transformed confidence intervals for many models.
    } else {
        dots <- list(...)
        if ("re.form" %in% names(dots)) {
            if (is.formula(re.form)) {
                include_random = re.form
            } else if (is.na(re.form)) {
                include_random = FALSE
            } else {
                include_random = TRUE
            }
        }
        pred <- insight::get_predicted(x,
                                       data = newdata,
                                       predict = type,
                                       ci = conf.level,
                                       include_random = include_random)





    return(out)
}
