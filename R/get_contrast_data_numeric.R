get_contrast_data_numeric <- function(model,
                                      newdata,
                                      variable,
                                      eps,
                                      ...) {


    x <- newdata[[variable$name]]

    make_label <- function(lab, val) {
        if (identical(lab, "custom")) return(lab)
        args <- append(list(lab), as.list(val))
        out <- tryCatch(
            do.call("sprintf", args),
            error = function(e) lab)
        return(out)
    }

    if (!is.null(eps)) {
        newdata$marginaleffects_eps <- eps
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
        "mean(dY/eX)")

    if (isTRUE(variable$label %in% slopes)) {
        low <- x
        high <- x + eps
        lab <- variable$label
        newdata$marginaleffects_eps <- eps

    } else if (identical(variable$label, "exp(dY/dX)")) {
        low <- x
        high <- x + eps
        lab <- "exp(dY/dX)"

    # contrast_label is designed for categorical predictors
    # numeric contrasts first
    } else if (isTRUE(checkmate::check_numeric(variable$value, len = 1))) {
        low <- x - variable$value / 2
        high <- x + variable$value / 2
        lab <- make_label("x + %s", variable$value)
        # wrap in parentheses, unless mean() because there are already parentheses
        # important to display ratios of x+1, etc.
        if (!isTRUE(grepl("mean", variable$label))) {
            lab <- make_label("(%s)", lab)
        }
        lab <- make_label(variable$label, c(lab, "x"))

    } else if (isTRUE(checkmate::check_numeric(variable$value, len = 2))) {
        variable$value <- sort(variable$value)
        low <- variable$value[1]
        high <- variable$value[2]
        gap <- diff(variable$value)
        lab <- make_label(variable$label, rev(variable$value))

    # character contrasts
    } else if (identical(variable$value, "sd")) {
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        low <- m - s / 2
        high <- m + s / 2
        lab <- c("x + sd/2", "x - sd/2")
        if (!isTRUE(grepl("mean", variable$label))) {
            lab <- sprintf("(%s)", lab)
        }
        lab <- make_label(variable$label, lab)

    } else if (identical(variable$value, "2sd")) {
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        low <- m - s
        high <- m + s
        lab <- c("x - sd", "x + sd")
        if (!isTRUE(grepl("mean", variable$label))) {
            lab <- sprintf("(%s)", lab)
        }
        lab <- make_label(variable$label, lab)

    } else if (identical(variable$value, "iqr")) {
        low <- stats::quantile(x, probs = .25, na.rm = TRUE)
        high <- stats::quantile(x, probs = .75, na.rm = TRUE)
        lab <- make_label(variable$label, c("Q3", "Q1"))

    } else if (identical(variable$value, "minmax")) {
        low <- min(x, na.rm = TRUE)
        high <- max(x, na.rm = TRUE)
        lab <- make_label(variable$label, c("Max", "Min"))
    }

    newdata[["eps"]] <- eps

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
