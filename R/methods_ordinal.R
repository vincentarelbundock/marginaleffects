#' @rdname get_predict
#' @export
get_predict.clm <- function(model,
                            newdata = insight::get_data(model),
                            vcov = FALSE,
                            conf_level = 0.95,
                            type = "response",
                            ...) {

    if (type == "response") {
        type <- "prob"
    }

    # `predict.clm()` only makes predictions for the observed response group of
    # each observation in `newdata`. When we remove the response from
    # `newdata`, `predict.clm()` makes predictions for all levels, which is
    # what we want.
    resp <- insight::find_response(model)

    # otherwise `predict.clm` does not see some columns (mystery)
    setDF(newdata)

    newdata <- newdata[, setdiff(colnames(newdata), resp), drop = FALSE]

    pred <- stats::predict(model,
                           newdata = newdata,
                           type = type)$fit

    out <- data.frame(
        group = rep(colnames(pred), each = nrow(pred)),
        estimate = c(pred))

    # often an internal call
    if ("rowid" %in% colnames(newdata)) {
        out$rowid <- rep(newdata$rowid, times = ncol(pred))
    } else {
        out$rowid <- rep(1:nrow(pred), times = ncol(pred))
    }

    return(out)
}

#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.clm <- get_group_names.polr


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
sanitize_model_specific.clm <- function(model, ...) {

    # Corner case: The `predict.clm` method does not make predictions when the
    # response was transformed to a factor in the formula AND the response is
    # missing from `newdata`.
    lhs <- names(attr(stats::terms(model), "dataClasses"))[1]
    if (isTRUE(grepl("^factor\\(", lhs))) {
        stop("The response variable should not be transformed to a factor in the formula. Please convert the variable to factor before fitting your model.",
             call. = FALSE)
    }
    return(model)
}
