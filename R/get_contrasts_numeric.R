get_contrast_data_numeric <- function(model,
                                      newdata,
                                      variable,
                                      contrast_numeric,
                                      contrast_label,
                                      eps,
                                      ...) {

    # contrast_label is designed for categorical predictors
    # numeric contrasts first
    if (is.numeric(contrast_numeric) && length(contrast_numeric) == 1) {
        low <- newdata[[variable]] - contrast_numeric / 2
        high <- newdata[[variable]] + contrast_numeric / 2
        lab <- sprintf("x + %s", contrast_numeric)
        if (!isTRUE(grepl("mean", contrast_label))) {
            lab <- sprintf("(%s)", lab)
        }
        lab <- sprintf(contrast_label, lab, "x")

    } else if (is.numeric(contrast_numeric) && length(contrast_numeric) == 2) {
        contrast_numeric <- sort(contrast_numeric)
        low <- contrast_numeric[1]
        high <- contrast_numeric[2]
        gap <- diff(contrast_numeric)
        lab <- sprintf(contrast_label, contrast_numeric[2], contrast_numeric[1])

    # character contrasts
    # slope
    } else if (isTRUE(contrast_numeric == "dydx")) {
        low <- newdata[[variable]]
        high <- newdata[[variable]] + eps
        lab <- "dydx"

    # other contrasts
    } else if (isTRUE(contrast_numeric == "sd")) {
        low <- mean(newdata[[variable]], na.rm = TRUE) - stats::sd(newdata[[variable]], na.rm = TRUE) / 2
        high <- mean(newdata[[variable]], na.rm = TRUE) + stats::sd(newdata[[variable]], na.rm = TRUE) / 2
        lab <- c("x - sd/2", "x + sd/2")
        if (!isTRUE(grepl("mean", contrast_label))) {
            lab <- sprintf("(%s)", lab)
        }
        lab <- sprintf(contrast_label, lab[2], lab[1])

    } else if (isTRUE(contrast_numeric == "2sd")) {
        low <- mean(newdata[[variable]], na.rm = TRUE) - stats::sd(newdata[[variable]], na.rm = TRUE)
        high <- mean(newdata[[variable]], na.rm = TRUE) + stats::sd(newdata[[variable]], na.rm = TRUE)
        lab <- c("x - sd", "x + sd")
        if (!isTRUE(grepl("mean", contrast_label))) {
            lab <- sprintf("(%s)", lab)
        }
        lab <- sprintf(contrast_label, lab[2], lab[1])

    } else if (isTRUE(contrast_numeric == "iqr")) {
        low <- stats::quantile(newdata[[variable]], probs = .25, na.rm = TRUE)
        high <- stats::quantile(newdata[[variable]], probs = .75, na.rm = TRUE)
        lab <- sprintf(contrast_label, "Q3", "Q1")

    } else if (isTRUE(contrast_numeric == "minmax")) {
        low <- min(newdata[[variable]], na.rm = TRUE)
        high <- max(newdata[[variable]], na.rm = TRUE)
        lab <- sprintf(contrast_label, "Max", "Min")
    }

    lo <- hi <- newdata
    lo[[variable]] <- low
    hi[[variable]] <- high
    out <- list(rowid = seq_len(nrow(newdata)),
                lo = lo,
                hi = hi,
                original = newdata,
                ter = rep(variable, nrow(newdata)),
                lab = rep(lab, nrow(newdata)))
    return(out)
}
