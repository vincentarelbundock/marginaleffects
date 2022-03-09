get_contrasts_character <- function(model,
                                    newdata,
                                    variable,
                                    type = "response",
                                    contrast_numeric = 1, # do not push forward
                                    contrast_factor = "reference",
                                    ...) {

    # factors store all levels, but characters do not, so we need to extract the
    # original data from the model.
    tmp <- insight::get_data(model)
    levs <- sort(unique(tmp[[variable]]))

    pred_list <- list()
    baseline <- newdata
    baseline[[variable]] <- levs[1]
    baseline_prediction <- get_predict(model,
                                       newdata = baseline,
                                       type = type,
                                       ...)

    draws_list <- list()
    for (i in 2:length(levs)) {
        pred <- baseline
        pred[[variable]] <- levs[i]
        incremented_prediction <- get_predict(model = model,
                                              newdata = pred,
                                              type = type,
                                              ...)

        contr <- as.vector(incremented_prediction$predicted) -
                 as.vector(baseline_prediction$predicted)

        # bayes: posterior draws and credible intervals
        if ("posterior_draws" %in% names(attributes(baseline_prediction))) {
            draws_list[[i]] <- attr(incremented_prediction, "posterior_draws") -
                               attr(baseline_prediction, "posterior_draws")
        }

        tmp <- incremented_prediction
        tmp$predicted <- NULL
        tmp$term <- variable
        tmp$contrast <- sprintf("%s - %s", levs[i], levs[1])
        tmp$comparison <- contr
        pred_list[[i - 1]] <- tmp
    }

    pred <- do.call("rbind", pred_list)
    draws <- do.call("rbind", draws_list)
    cols <- intersect(c("rowid", "group", "term", "contrast", "comparison"), colnames(pred))
    pred <- pred[, cols]
    row.names(pred) <- NULL
    attr(pred, "posterior_draws") <- draws
    return(pred)
}

