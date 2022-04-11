get_contrast_data_numeric <- function(model,
                                      newdata,
                                      variable,
                                      contrast_numeric = 1,
                                      ...) {


    eps <- getOption("marginaleffects_deriv_eps", default = 1e-5)

    # slope
    if (contrast_numeric == "dydx") {
        low <- newdata[[variable]]
        high <- newdata[[variable]] + eps
        lab <- "dydx"

    # other contrasts
    } else if (is.numeric(contrast_numeric) && length(contrast_numeric) == 1) {
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
