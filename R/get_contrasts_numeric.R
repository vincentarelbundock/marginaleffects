get_contrasts_numeric <- function(model,
                                  newdata,
                                  variable,
                                  type = "response",
                                  contrast_numeric = 1,
                                  contrast_numeric_slope = FALSE,
                                  ...) {

    baseline <- newdata

    pred_baseline <- get_predict(model,
                                 newdata = baseline,
                                 type = type,
                                 ...)

    baseline[[variable]] <- baseline[[variable]] + contrast_numeric
    pred_increment <- get_predict(model,
                                  newdata = baseline,
                                  type = type,
                                  ...)

    contr <- as.vector(pred_increment$predicted) - as.vector(pred_baseline$predicted)

    pred_increment$term <- variable

    if (isTRUE(contrast_numeric_slope)) {
        contr <- contr / contrast_numeric
        pred_increment$contrast <- "dydx"
        pred_increment$estimate <- contr
    } else {
        pred_increment$contrast <- sprintf("+%s", contrast_numeric)
        pred_increment$estimate <- contr
    }
    pred_increment$predicted <- NULL

    out <- pred_increment

    # bayes: posterior draws and credible intervals
    if ("posterior_draws" %in% names(attributes(pred_increment))) {
        draws <- attr(pred_increment, "posterior_draws") - attr(pred_baseline, "posterior_draws")
        if (isTRUE(contrast_numeric_slope)) {
            attr(out, "posterior_draws") <- draws / contrast_numeric
        } else {
            attr(out, "posterior_draws") <- draws
        }
    }

    return(out)
}
