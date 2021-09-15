#' Compute marginal effects using differences in predicted values between categorical variables (internal function)
#'
#' @rdname get_dydx_categorical
#' @inheritParams marginaleffects
#' @inheritParams get_dydx_and_se
#' @keywords internal
#' @export
get_dydx_categorical <- function (model, ...) {
    UseMethod("get_dydx_categorical", model)
}


#' @export
#' @keywords internal
get_dydx_categorical.default <- function(model,
                                         variable,
                                         fitfram = insight::get_data(model),
                                         group_name = NULL,
                                         predict_type = "response",
                                         ...) {

    args <- list(model = model)
    if (is.factor(fitfram[[variable]])) {
        args[[variable]] <- levels(fitfram[[variable]])
    } else if (is.logical(fitfram[[variable]])) {
        args[[variable]] <- c(FALSE, TRUE)
    } else if (is.character(fitfram[[variable]])) {
        args[[variable]] <- unique(fitfram[[variable]])
    } else {
        stop(sprintf('"%s" is not a factor, logical, or character.', variable))
    }

    # TODO: This is certainly inefficient. Just create two near-identical datasets and compare the predictions instead of doing all this sorting.
    pred <- do.call("counterfactual", args)
    pred$predicted <- get_predict(model = model, newdata = pred, predict_type = predict_type)
    colnames(pred)[match(variable, colnames(pred))] <- "variable"
    pred <- pred[order(pred$rowid, pred$variable), ]
    pred <- dplyr::group_by(pred, rowid)
    pred <- dplyr::mutate(pred, dydx = predicted - predicted[1])
    pred <- dplyr::slice(pred, -1)
    pred <- dplyr::select(pred, rowid, variable, dydx)
    pred <- dplyr::ungroup(pred)
    pred$variable <- as.character(pred$variable)
    pred <- as.data.frame(pred)

    # two possible label formats for factor level coefficients: factor(cyl)4 vs. cyl4
    lab_fmt1 <- sprintf("factor(%s)%s", variable, args[[variable]][2:length(args[[variable]])])
    lab_fmt2 <- sprintf("%s%s", variable, args[[variable]][2:length(args[[variable]])])
    if (all(lab_fmt1 %in% names(get_coef(model)))) {
        pred$variable <- sprintf("factor(%s)%s", variable, pred$variable)
    } else if (all(lab_fmt2 %in% names(get_coef(model)))) {
        pred$variable <- sprintf("%s%s", variable, pred$variable)
    } else {
        return(NULL)
        # stop(sprintf("Could not identify the coefficients that match the levels of variable %s.", variable)) 
    }

    colnames(pred)[match("variable", colnames(pred))] <- "term"

    return(pred)
}
