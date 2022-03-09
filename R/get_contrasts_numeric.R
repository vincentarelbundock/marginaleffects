get_contrasts_numeric <- function(model,
                                  newdata,
                                  variable,
                                  type = "response",
                                  contrast_numeric = 1,
                                  ...) {

    if (is.numeric(contrast_numeric) && length(contrast_numeric) == 1) {
        low <- newdata[[variable]]
        high <- newdata[[variable]] + contrast_numeric
        lab <- sprintf("+%s", contrast_numeric)
    } else if (is.numeric(contrast_numeric) && length(contrast_numeric) == 2) {
        contrast_numeric <- sort(contrast_numeric)
        low <- contrast_numeric[1]
        high <- contrast_numeric[2]
        gap <- diff(contrast_numeric)
        lab  <- paste(contrast_numeric[2], "-", contrast_numeric[1])
    } else if (contrast_numeric == "sd") {
        low <- mean(newdata[[variable]], na.rm = TRUE) - stats::sd(newdata[[variable]], na.rm = TRUE) / 2
        high <- mean(newdata[[variable]], na.rm = TRUE) + stats::sd(newdata[[variable]], na.rm = TRUE) / 2
        lab <- "sd"
    } else if (contrast_numeric == "2sd") {
        low <- mean(newdata[[variable]], na.rm = TRUE) - stats::sd(newdata[[variable]], na.rm = TRUE)
        high <- mean(newdata[[variable]], na.rm = TRUE) + stats::sd(newdata[[variable]], na.rm = TRUE)
        lab <- "2sd"
    } else if (contrast_numeric == "iqr") {
        low <- stats::quantile(newdata[[variable]], probs = .25, na.rm = TRUE)
        high <- stats::quantile(newdata[[variable]], probs = .75, na.rm = TRUE)
        lab <- "IQR"
    } else if (contrast_numeric == "minmax") {
        low <- min(newdata[[variable]], na.rm = TRUE)
        high <- max(newdata[[variable]], na.rm = TRUE)
        lab <- "Max - Min"
    }
    gap <- high - low

    baseline <- newdata

    baseline[[variable]] <- low
    pred_baseline <- get_predict(model,
                                 newdata = baseline,
                                 type = type,
                                 ...)

    baseline[[variable]] <- high
    pred_increment <- get_predict(model,
                                  newdata = baseline,
                                  type = type,
                                  ...)

    contr <- as.vector(pred_increment$predicted) - as.vector(pred_baseline$predicted)

    pred_increment$term <- variable

    # secret argument
    if (isTRUE(list(...)[["contrast_numeric_slope"]])) {
        contr <- contr / gap
        pred_increment$contrast <- "dydx"
        pred_increment$comparison <- contr
    } else {
        pred_increment$contrast <- lab
        pred_increment$comparison <- contr
    }
    pred_increment$predicted <- NULL

    out <- pred_increment

    # bayes: posterior draws and credible intervals
    if ("posterior_draws" %in% names(attributes(pred_increment))) {
        draws <- attr(pred_increment, "posterior_draws") - attr(pred_baseline, "posterior_draws")
        if (isTRUE(list(...)[["contrast_numeric_slope"]])) {
            attr(out, "posterior_draws") <- draws / contrast_numeric
        } else {
            attr(out, "posterior_draws") <- draws
        }
    }

    return(out)
}
