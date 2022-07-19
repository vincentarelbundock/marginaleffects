get_contrast_data_numeric <- function(model,
                                      newdata,
                                      variable,
                                      eps,
                                      ...) {


    x <- newdata[[variable$name]]

    # slope
    # by default variable$value = 1, so we need to check this first
    if (isTRUE(variable$label %in% c("dydx", "eyex", "eydx", "dyex"))) {
        low <- x
        high <- x + eps
        lab <- variable$label
        newdata$marginaleffects_x <- x
        newdata$marginaleffects_eps <- eps


    } else if (identical(variable$label, "expdydx")) {
        low <- x
        high <- x + eps
        lab <- "exp(dY/dX)"

    # contrast_label is designed for categorical predictors
    # numeric contrasts first
    } else if (isTRUE(checkmate::check_numeric(variable$value, len = 1))) {
        low <- x - variable$value / 2
        high <- x + variable$value / 2
        lab <- sprintf("x + %s", variable$value)
        if (!isTRUE(grepl("mean", variable$label))) {
            lab <- sprintf("(%s)", lab)
        }
        lab <- sprintf(variable$label, lab, "x")

    } else if (isTRUE(checkmate::check_numeric(variable$value, len = 2))) {
        variable$value <- sort(variable$value)
        low <- variable$value[1]
        high <- variable$value[2]
        gap <- diff(variable$value)
        lab <- sprintf(variable$label, variable$value[2], variable$value[1])

    # character contrasts
    } else if (identical(variable$value, "sd")) {
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        low <- m - s / 2
        high <- m + s / 2
        lab <- c("x - sd/2", "x + sd/2")
        if (!isTRUE(grepl("mean", variable$label))) {
            lab <- sprintf("(%s)", lab)
        }
        lab <- sprintf(variable$label, lab[2], lab[1])

    } else if (identical(variable$value, "2sd")) {
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        low <- m - s
        high <- m + s
        lab <- c("x - sd", "x + sd")
        if (!isTRUE(grepl("mean", variable$label))) {
            lab <- sprintf("(%s)", lab)
        }
        lab <- sprintf(variable$label, lab[2], lab[1])

    } else if (identical(variable$value, "iqr")) {
        low <- stats::quantile(x, probs = .25, na.rm = TRUE)
        high <- stats::quantile(x, probs = .75, na.rm = TRUE)
        lab <- sprintf(variable$label, "Q3", "Q1")

    } else if (identical(variable$value, "minmax")) {
        low <- min(x, na.rm = TRUE)
        high <- max(x, na.rm = TRUE)
        lab <- sprintf(variable$label, "Max", "Min")
    }

    lo <- hi <- newdata
    lo[[variable$name]] <- low
    hi[[variable$name]] <- high
    out <- list(rowid = seq_len(nrow(newdata)),
                lo = lo,
                hi = hi,
                original = newdata,
                ter = rep(variable$name, nrow(newdata)),
                lab = rep(lab, nrow(newdata)))
    return(out)
}
