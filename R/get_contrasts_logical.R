get_contrasts_logical <- function(model,
                                  newdata,
                                  variable,
                                  type = "response",
                                  contrast_numeric = 1, # do not push forward
                                  contrast_factor = "reference",
                                  ...) {

    baseline <- newdata
    baseline[[variable]] <- FALSE
    pred_false <- get_predict(model,
                              newdata = baseline,
                              type = type,
                              ...)
    baseline[[variable]] <- TRUE
    pred_true <- get_predict(model = model,
                             newdata = baseline,
                             type = type,
                             ...)

    baseline$comparison <- pred_true$predicted - pred_false$predicted
    baseline$term <- variable
    baseline$contrast <- "TRUE - FALSE"
    cols <- intersect(colnames(baseline),
                      c("rowid", "group", "term", "contrast", "comparison"))
    pred <- baseline[, cols]
    row.names(pred) <- NULL

    # bayes: posterior draws and credible intervals
    if ("posterior_draws" %in% names(attributes(pred_false))) {
        attr(pred, "posterior_draws") <- attr(pred_true, "posterior_draws") - attr(pred_false, "posterior_draws")
    }

    return(pred)
}

