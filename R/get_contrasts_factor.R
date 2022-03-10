get_contrasts_factor <- function(model,
                                 newdata,
                                 variable,
                                 type = "response",
                                 contrast_numeric = 1, # do not push forward
                                 contrast_factor = "reference",
                                 ...) {

    baseline <- newdata
    pred_list <- list()
    if (is.factor(baseline[[variable]])) {
        levs <- levels(baseline[[variable]])
        convert_to_factor <- TRUE
    } else {
        msg <- sprintf("The `%s` variable is treated as a categorical (factor) variable, but the original data is of class %s. It is safer and faster to convert such variables to factor before fitting the model and calling `marginaleffects` functions.", variable, class(baseline[[variable]])[1])
        warn_once(msg, "marginaleffects_warning_factor_on_the_fly_conversion")

        original_data <- insight::get_data(model)
        if (is.factor(original_data[[variable]])) {
            levs <- levels(original_data[[variable]])
            convert_to_factor <- TRUE
        } else {
            levs <- sort(unique(original_data[[variable]]))
            convert_to_factor <- FALSE
        }
    }

    # index contrast orders based on contrast_factor
    if (contrast_factor == "reference") {
        levs_idx <- data.frame(low = levs[1],
                              high = levs[2:length(levs)])
    } else if (contrast_factor == "pairwise") {
        levs_idx <- expand.grid(low = levs,
                               high = levs,
                               stringsAsFactors = FALSE)
        levs_idx <- levs_idx[levs_idx$high != levs_idx$low,]
        levs_idx <- levs_idx[match(levs_idx$low, levs) < match(levs_idx$high, levs),]
    } else if (contrast_factor == "sequential") {
        levs_idx <- data.frame(low = levs[1:(length(levs) - 1)],
                              high = levs[2:length(levs)])
    }

    levs_idx$label <- sprintf("%s - %s", levs_idx$high, levs_idx$low)

    draws_list <- list()

    for (i in seq_len(nrow(levs_idx))) {

        # when factor() is called in a formula and the original data is numeric
        if (isTRUE(convert_to_factor)) {
            baseline[[variable]] <- factor(levs_idx[i, "low"], levels = levs)
        } else {
            baseline[[variable]] <- levs_idx[i, "low"]
        }
        baseline_prediction <- get_predict(model,
                                           newdata = baseline,
                                           type = type,
                                           ...)

        # when factor() is called in a formula and the original data is numeric
        if (isTRUE(convert_to_factor)) {
            baseline[[variable]] <- factor(levs_idx[i, "high"], levels = levs)
        } else {
            baseline[[variable]] <- levs_idx[i, "high"]
        }

        incremented_prediction <- get_predict(model = model,
                                              newdata = baseline,
                                              type = type,
                                              ...)
        incremented_prediction$term <- variable
        incremented_prediction$contrast <- levs_idx[i, "label"]
        incremented_prediction$comparison <- incremented_prediction$predicted -
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
    cols <- intersect(colnames(pred), c("rowid", "group", "term", "contrast", "comparison"))
    row.names(pred) <- NULL
    attr(pred, "posterior_draws") <- draws

    return(pred)
}

