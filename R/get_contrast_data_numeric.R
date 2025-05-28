get_contrast_data_numeric <- function(
    model,
    newdata,
    variable,
    modeldata,
    ...
) {
    h <- variable[["eps"]]

    s <- m <- NA
    if (is.numeric(modeldata[[variable$name]])) {
        s <- stats::sd(modeldata[[variable$name]], na.rm = TRUE)
        m <- mean(modeldata[[variable$name]], na.rm = TRUE)
    }
    x <- newdata[[variable$name]]
    xmd <- modeldata[[variable$name]]

    make_label <- function(lab, val) {
        if (identical(lab, "custom")) return(lab)
        args <- append(list(lab), as.list(val))
        out <- tryCatch(
            do.call("sprintf", args),
            error = function(e) lab
        )
        return(out)
    }

    # slope
    # by default variable$value, so we need to check this first
    slopes <- c(
        "dY/dX",
        "eY/eX",
        "eY/dX",
        "dY/eX",
        "mean(dY/dX)",
        "mean(eY/eX)",
        "mean(eY/dX)",
        "mean(dY/eX)"
    )

    # manual high
    if (isTRUE(checkmate::check_data_frame(variable$value))) {
        if (all(c("low", "high") %in% colnames(variable$value))) {
            low <- variable$value$low
            high <- variable$value$high
        } else {
            low <- variable$value[[1]]
            high <- variable$value[[2]]
        }
        lab <- "manual"
    } else if (isTRUE(variable$label %in% slopes)) {
        low <- x - h / 2
        high <- x + h / 2
        lab <- variable$label
    } else if (identical(variable$label, "exp(dY/dX)")) {
        low <- x - h / 2
        high <- x + h / 2
        lab <- "exp(dY/dX)"

        # contrast_label is designed for categorical predictors
        # numeric contrasts first
    } else if (isTRUE(checkmate::check_numeric(variable$value, len = 1))) {
        direction <- getOption(
            "marginaleffects_contrast_direction",
            default = "forward"
        )
        if (isTRUE(direction == "center")) {
            low <- x - variable$value / 2
            high <- x + variable$value / 2
        } else if (isTRUE(direction == "backward")) {
            low <- x - variable$value
            high <- x
        } else {
            low <- x
            high <- x + variable$value
        }

        # wrap in parentheses, unless mean() because there are already parentheses
        # important to display ratios of x+1, etc.
        # label should not be `(mpg+1) - mpg` because that is misleading for centered contrast
        if (!isTRUE(grepl("mean", variable$label))) {
            lab <- sprintf("+%s", variable$value)
        } else {
            lab <- sprintf("mean(+%s)", variable$value)
        }
    } else if (isTRUE(checkmate::check_numeric(variable$value, len = 2))) {
        variable$value <- sort(variable$value)
        low <- variable$value[1]
        high <- variable$value[2]
        gap <- diff(variable$value)
        lab <- make_label(variable$label, rev(variable$value))

        # character contrasts
    } else if (identical(variable$value, "sd")) {
        low <- m - s / 2
        high <- m + s / 2
        lab <- c("x + sd/2", "x - sd/2")
        if (!isTRUE(grepl("mean", variable$label))) {
            lab <- sprintf("(%s)", lab)
        }
        lab <- make_label(variable$label, lab)
    } else if (identical(variable$value, "2sd")) {
        low <- m - s
        high <- m + s
        lab <- c("x + sd", "x - sd")
        if (!isTRUE(grepl("mean", variable$label))) {
            lab <- sprintf("(%s)", lab)
        }
        lab <- make_label(variable$label, lab)
    } else if (identical(variable$value, "iqr")) {
        low <- stats::quantile(xmd, probs = 0.25, na.rm = TRUE)
        high <- stats::quantile(xmd, probs = 0.75, na.rm = TRUE)
        lab <- make_label(variable$label, c("Q3", "Q1"))
    } else if (identical(variable$value, "minmax")) {
        low <- min(xmd, na.rm = TRUE)
        high <- max(xmd, na.rm = TRUE)
        lab <- make_label(variable$label, c("Max", "Min"))
    } else if (isTRUE(checkmate::check_function(variable$value))) {
        tmp <- variable$value(x)
        low <- tmp[, 1]
        high <- tmp[, 2]
        lab <- "custom"
    }

    lo <- hi <- newdata
    lo[[variable$name]] <- low
    hi[[variable$name]] <- high
    out <- list(
        rowid = seq_len(nrow(newdata)),
        lo = lo,
        hi = hi,
        original = newdata,
        ter = rep(variable$name, nrow(newdata)),
        lab = rep(lab, nrow(newdata))
    )
    return(out)
}
