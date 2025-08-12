get_ci <- function(
    x,
    conf_level,
    df = NULL,
    draws = NULL,
    vcov = TRUE,
    hypothesis_null = 0,
    hypothesis_direction = "=",
    model = NULL,
    ...) {
    checkmate::assert_number(hypothesis_null)

    if (!is.null(draws)) {
        out <- get_ci_draws(
            x,
            conf_level = conf_level,
            draws = draws,
            model = model
        )
        return(out)
    }

    required <- c("estimate", "std.error")
    if (!inherits(x, "data.frame") || !all(required %in% colnames(x))) {
        return(x)
    }

    normal <- FALSE
    if (!"df" %in% colnames(x)) {
        if (identical(df, Inf)) {
            normal <- TRUE

            # 1 or matching length
        } else if (length(df) %in% c(1, nrow(x))) {
            x[["df"]] <- df
            normal <- FALSE

            # multiple, such as rbind() contrast terms
        } else if (length(df) < nrow(x) && "rowid" %in% colnames(x)) {
            rowids <- unique(x$rowid)
            if (length(rowids) == length(df)) {
                rowids <- data.table(rowid = rowids, df = df)
                x <- merge(x, rowids, all.x = TRUE, by = "rowid", sort = FALSE)
            } else {
                insight::format_error("The degrees of freedom argument was ignored.")
            }

            # mismatch
        } else {
            stop(
                "Please report this error with a fully reproducible example at: https://github.com/vincentarelbundock/marginaleffects"
            )
        }
    }

    p_overwrite <- !"p.value" %in% colnames(x) ||
        hypothesis_null != 0 ||
        hypothesis_direction != "=" ||
        identical(vcov, "satterthwaite") ||
        identical(vcov, "kenward-roger")

    z_overwrite <- !"statistic" %in% colnames(x) ||
        hypothesis_null != 0 ||
        p_overwrite

    ci_overwrite <- !"conf.low" %in% colnames(x) &&
        "std.error" %in% colnames(x)

    if (z_overwrite) {
        cdf <- function(k) {
            if (normal) {
                out <- stats::pnorm(k)
            } else {
                out <- stats::pt(k, df = x[["df"]])
            }
            return(out)
        }
        x[["statistic"]] <- (x[["estimate"]] - hypothesis_null) / x[["std.error"]]
        if (hypothesis_direction == "=") {
            x[["p.value"]] <- 2 * cdf(-abs(x$statistic))
        } else if (hypothesis_direction == "<=") {
            x[["p.value"]] <- 1 - cdf(x$statistic)
        } else if (hypothesis_direction == ">=") {
            x[["p.value"]] <- cdf(x$statistic)
        }
    }

    if (ci_overwrite) {
        alpha <- 1 - conf_level
        if (normal) {
            critical <- abs(stats::qnorm(alpha / 2))
        } else {
            critical <- abs(stats::qt(alpha / 2, df = x[["df"]]))
        }
        x[["conf.low"]] <- x[["estimate"]] - critical * x[["std.error"]]
        x[["conf.high"]] <- x[["estimate"]] + critical * x[["std.error"]]
    }

    # s-value
    if ("p.value" %in% colnames(x)) {
        x$s.value <- -log2(x$p.value)
    }

    return(x)
}


get_ci_draws <- function(x, conf_level, draws, model = NULL) {
    checkmate::check_number(conf_level, lower = 1e-10, upper = 1 - 1e-10)
    critical <- (1 - conf_level) / 2

    # faster known case
    if (inherits(model, "inferences_simulation")) {
        insight::check_if_installed("collapse", minimum_version = "1.9.0")
        CIs <- collapse::dapply(
            draws,
            MARGIN = 1,
            FUN = collapse::fquantile,
            probs = c(critical, 1 - critical)
        )
        x$std.error <- collapse::dapply(draws, MARGIN = 1, FUN = collapse::fsd)
        x$conf.low <- CIs[, 1]
        x$conf.high <- CIs[, 2]
        return(x)
    } else if (
        identical(
            "eti",
            getOption("marginaleffects_posterior_interval", default = "eti")
        ) &&
            identical(
                "median",
                getOption("marginaleffects_posterior_center", default = "median")
            )
    ) {
        insight::check_if_installed("collapse", minimum_version = "1.9.0")
        # Issue #1017
        if (nrow(draws) > 0) {
            CIs <- collapse::dapply(
                draws,
                MARGIN = 1,
                FUN = collapse::fquantile,
                probs = c(critical, 0.5, 1 - critical)
            )
            x$estimate <- CIs[, 2]
            x$conf.low <- CIs[, 1]
            x$conf.high <- CIs[, 3]
        }
        return(x)
    }

    # faster known case
    if (
        identical(
            "eti",
            getOption("marginaleffects_posterior_interval", default = "eti")
        ) &&
            identical(
                "mean",
                getOption("marginaleffects_posterior_center", default = "median")
            )
    ) {
        insight::check_if_installed("collapse", minimum_version = "1.9.0")
        Bs <- collapse::dapply(draws, MARGIN = 1, FUN = collapse::fmean)
        CIs <- collapse::dapply(
            draws,
            MARGIN = 1,
            FUN = collapse::fquantile,
            probs = c(critical, 1 - critical)
        )
        x$estimate <- Bs
        x$conf.low <- CIs[, 1]
        x$conf.high <- CIs[, 2]
        return(x)
    }

    # option name change
    FUN_INTERVAL <- getOption("marginaleffects_posterior_interval")
    if (is.null(FUN_INTERVAL)) {
        FUN_INTERVAL <- getOption(
            "marginaleffects_credible_interval",
            default = "eti"
        )
    }
    checkmate::assert_choice(FUN_INTERVAL, choices = c("eti", "hdi"))
    if (FUN_INTERVAL == "hdi") {
        FUN_INTERVAL <- get_hdi
    } else {
        FUN_INTERVAL <- get_eti
    }

    FUN_CENTER <- getOption(
        "marginaleffects_posterior_center",
        default = stats::median
    )

    checkmate::assert(
        checkmate::check_choice(FUN_CENTER, choices = c("mean", "median")),
        checkmate::check_function(FUN_CENTER)
    )

    if (identical(FUN_CENTER, "mean")) {
        FUN_CENTER <- mean
    } else if (identical(FUN_CENTER, "median")) {
        FUN_CENTER <- stats::median
    }

    # necessary for `hypothesis_apply()` when attributes are added by collapse::BY or collapse::dapply
    colnames(draws) <- row.names(draws) <- NULL

    CIs <- t(apply(draws, 1, FUN_INTERVAL, credMass = conf_level))
    Bs <- apply(draws, 1, FUN_CENTER)
    # comparison returns a single value
    if (nrow(x) < nrow(CIs)) {
        CIs <- unique(CIs)
        Bs <- unique(Bs)
    }
    x[["estimate"]] <- Bs
    x[["conf.low"]] <- CIs[, "lower"]
    x[["conf.high"]] <- CIs[, "upper"]

    return(x)
}


get_eti <- function(object, credMass = 0.95, ...) {
    checkmate::assert_numeric(object)
    checkmate::assert_number(credMass)
    checkmate::assert_true(credMass > 0)
    checkmate::assert_true(credMass < 1)
    critical <- (1 - credMass) / 2
    out <- stats::quantile(object, probs = c(critical, 1 - critical))
    out <- stats::setNames(out, c("lower", "upper"))
    return(out)
}


# this is only used for tests to match emmeans. we use ETI as default for bayesian models.
get_hdi <- function(object, credMass = 0.95, ...) {
    result <- c(NA_real_, NA_real_)
    if (is.numeric(object)) {
        attributes(object) <- NULL
        x <- sort.int(object, method = "quick") # removes NA/NaN, but not Inf
        n <- length(x)
        if (n > 0) {
            # exclude <- ceiling(n * (1 - credMass)) # Not always the same as...
            exclude <- n - floor(n * credMass) # Number of values to exclude
            low.poss <- x[1:exclude] # Possible lower limits...
            upp.poss <- x[(n - exclude + 1):n] # ... and corresponding upper limits
            best <- which.min(upp.poss - low.poss) # Combination giving the narrowest interval
            if (length(best)) {
                result <- c(low.poss[best], upp.poss[best])
            } else {
                tmp <- range(x)
                if (length(tmp) == 2) {
                    result <- tmp
                }
            }
        }
    }
    names(result) <- c("lower", "upper")
    return(result)
}
