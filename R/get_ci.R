get_ci <- function(
    x,
    conf_level,
    estimate,
    df = NULL,
    draws = NULL,
    overwrite = FALSE) {

    if (!is.null(draws)) {
        out <- get_ci_draws(
            x,
            conf_level = conf_level,
            estimate = estimate,
            draws = draws,
            overwrite = overwrite)
        return(out)
    }

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


get_ci_draws <- function(
    x,
    conf_level,
    draws,
    estimate,
    overwrite = FALSE) {

    flag <- getOption("marginaleffects_credible_interval", default = "eti")
    FUN <- ifelse(isTRUE(flag == "hdi"), get_hdi, get_eti)

    if (!"conf.low" %in% colnames(x) || isTRUE(overwrite)) {
        tmp <- apply(draws, 1, FUN, credMass = conf_level)
        x[["std.error"]] <- NULL
        x[[estimate]] <- apply(draws, 1, stats::median)
        x[["conf.low"]] <- tmp[1, ]
        x[["conf.high"]] <- tmp[2, ]
    }

    return(x)
}


