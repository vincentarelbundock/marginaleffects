get_contrasts_factor <- function(model,
                                 newdata,
                                 variable,
                                 type = "response",
                                 contrast_factor = "reference",
                                 ...) {

    baseline <- newdata
    pred_list <- list()
    if (is.factor(baseline[[variable]])) {
        levs <- levels(baseline[[variable]])
        convert_to_factor <- TRUE
    } else {
        original_data <- insight::get_data(model)
        if (is.factor(original_data[[variable]])) {
            levs <- levels(original_data[[variable]])
            convert_to_factor <- TRUE
        } else {
            levs <- sort(unique(original_data[[variable]]))
            convert_to_factor <- FALSE
        }
    }

    # when factor() is called in a formula and the original data is numeric
    if (isTRUE(convert_to_factor)) {
        baseline[[variable]] <- factor(levs[1], levels = levs)
    } else {
        baseline[[variable]] <- levs[1]
    }

    baseline_prediction <- get_predict(model,
                                       newdata = baseline,
                                       type = type,
                                       ...)
    draws_list <- list()

    for (i in 2:length(levs)) {
        # when factor() is called in a formula and the original data is numeric
        if (isTRUE(convert_to_factor)) {
            baseline[[variable]] <- factor(levs[i], levels = levs)
        } else {
            baseline[[variable]] <- levs[i]
        }

        incremented_prediction <- get_predict(model = model,
                                              newdata = baseline,
                                              type = type,
                                              ...)
        incremented_prediction$term <- variable
        incremented_prediction$contrast <- sprintf("%s - %s", levs[i], levs[1])
        incremented_prediction$estimate <- incremented_prediction$predicted -
                                           baseline_prediction$predicted
        incremented_prediction$predicted <- NULL
        pred_list[[i]] <- incremented_prediction

        # bayes: posterior draws and credible intervals
        if ("posterior_draws" %in% names(attributes(baseline_prediction))) {
            draws_list[[i]] <- attr(incremented_prediction, "posterior_draws") -
                               attr(baseline_prediction, "posterior_draws")
        }
    }
    pred <- do.call("rbind", pred_list)
    draws <- do.call("rbind", draws_list)

    # TODO: check if this is still relevant
    # # clean for hurdle models from package `pscl`
    # coef_names <- gsub("count_|zero_", "", coef_names)

    # output
    cols <- intersect(colnames(pred), c("rowid", "group", "term", "contrast", "estimate"))
    row.names(pred) <- NULL
    attr(pred, "posterior_draws") <- draws

    return(pred)
}

