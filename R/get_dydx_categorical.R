#' Compute marginal effects using differences in predicted values between categorical variables (internal function)
#'
#' @rdname get_dydx_categorical
#' @inheritParams marginaleffects
#' @inheritParams get_dydx_and_se
#' @keywords internal
#' @return Numeric vector of contrasts associated to a categorical regressor
get_dydx_categorical <- function (model, ...) {
    UseMethod("get_dydx_categorical", model)
}


#' @keywords internal
get_dydx_categorical.default <- function(model,
                                         variable,
                                         fitfram = insight::get_data(model),
                                         group_name = NULL,
                                         type = "response",
                                         ...) {

    # Create counterfactual datasets with different factor values and compare the predictions
    if (!"rowid" %in% colnames(fitfram)) {
        fitfram$rowid <- 1:nrow(fitfram)
    }
    baseline <- fitfram

    if (is.logical(baseline[[variable]])) {
        baseline[[variable]] <- FALSE
        baseline_prediction <- get_predict(model, 
                                           newdata = baseline, 
                                           type = type,
                                           group_name = group_name)
        baseline[[variable]] <- TRUE
        baseline$predicted <- get_predict(model = model, 
                                          newdata = baseline, 
                                          type = type,
                                          group_name = group_name) - baseline_prediction
        baseline$term <- paste0(variable, baseline[[variable]])
        pred <- baseline[, c("rowid", "term", "predicted")]
    }

    if (is.factor(baseline[[variable]])) {
        pred_list <- list()
        baseline[[variable]] <- factor(levels(baseline[[variable]])[1], levels = levels(baseline[[variable]]))
        baseline_prediction <- get_predict(model, 
                                           newdata = baseline, 
                                           type = type,
                                           group_name = group_name)
        for (i in 2:length(levels(baseline[[variable]]))) {
            baseline[[variable]] <- factor(levels(baseline[[variable]])[i], levels = levels(baseline[[variable]]))
            baseline$predicted <- get_predict(model = model, 
                                              newdata = baseline, 
                                              type = type,
                                              group_name = group_name) - baseline_prediction
            pred_list[[i]] <- baseline[, c("rowid", variable, "predicted")]
        }
        pred <- do.call("rbind", pred_list)

        # two possible label formats for factor level coefficients: factor(cyl)4 vs. cyl4
        levs <- levels(fitfram[[variable]])
        levs <- levs[2:length(levs)]
        lab_fmt1 <- sprintf("factor(%s)%s", variable, levs)
        lab_fmt2 <- sprintf("%s%s", variable, levs)
        if (all(lab_fmt1 %in% names(get_coef(model)))) {
            pred$term <- sprintf("factor(%s)%s", variable, pred[[variable]])
        } else if (all(lab_fmt2 %in% names(get_coef(model)))) {
            pred$term <- sprintf("%s%s", variable, pred[[variable]])
        } else {
            pred$term <- pred[[variable]]
        }
        pred <- pred[, c("rowid", "term", "predicted")]
    }

    if (is.character(baseline[[variable]])) {
        pred_list <- list()
        levs <- unique(baseline[[variable]])
        baseline[[variable]] <- levs[1]
        baseline_prediction <- get_predict(model, 
                                           newdata = baseline, 
                                           type = type,
                                           group_name = group_name)
        for (i in 2:length(levs)) {
            baseline[[variable]] <- levs[i]
            baseline$predicted <- get_predict(model = model, 
                                              newdata = baseline, 
                                              type = type,
                                              group_name = group_name) - baseline_prediction
            baseline$term <- sprintf("%s%s", variable, baseline[[variable]])
            pred_list[[i]] <- baseline[, c("rowid", "term", "predicted")]
        }
        pred <- do.call("rbind", pred_list)
    }

    # clean
    colnames(pred) <- c("rowid", "term", "dydx")
    row.names(pred) <- NULL

    return(pred)
}
