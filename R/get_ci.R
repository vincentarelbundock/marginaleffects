get_ci <- function(
    x,
    conf_level,
    estimate,
    df = NULL,
    overwrite = FALSE) {

    required <- c(estimate, "std.error")
    if (!inherits(x, "data.frame") || any(!required %in% colnames(x))) {
        return(x)
    }

    alpha <- 1 - conf_level

    if (is.null(df)) {
        critical <- abs(stats::qnorm(alpha / 2))
    } else {
        critical <- abs(stats::qt(alpha / 2, df = df))
        if (!"df" %in% colnames(x)) {
            x[["df"]] <- df
        }
    }

    if (!"statistic" %in% colnames(x) || isTRUE(overwrite)) {
        x[["statistic"]] <- x[[estimate]] / x[["std.error"]]
    }

    if (!"p.value" %in% colnames(x) || isTRUE(overwrite)) {
        if ("df" %in% colnames(x)) {
            x[["p.value"]] <- 2 * stats::pt(-abs(x$statistic), df = x[["df"]])
        } else {
            x[["p.value"]] <- 2 * stats::pnorm(-abs(x$statistic))
        }
    }

    if (!"conf.low" %in% colnames(x) || isTRUE(overwrite)) {
        x[["conf.low"]] <- x[[estimate]] - critical * x[["std.error"]]
        x[["conf.high"]] <- x[[estimate]] + critical * x[["std.error"]]
    }

    return(x)
}

