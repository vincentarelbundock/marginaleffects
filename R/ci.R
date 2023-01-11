get_ci <- function(
    x,
    conf_level,
    estimate,
    df = NULL,
    draws = NULL,
    vcov = TRUE,
    overwrite = FALSE,
    null = 0,
    ...) {


    if (!is.null(draws)) {
        out <- get_ci_draws(
            x,
            conf_level = conf_level,
            estimate = estimate,
            draws = draws,
            overwrite = overwrite)
        return(out)
    }

    checkmate::assert_numeric(null, len = 1)

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
        x[["statistic"]] <- (x[[estimate]] - null) / x[["std.error"]]
    }

    if (!"p.value" %in% colnames(x) || null != 0 || isTRUE(overwrite)) {
        if (!"df" %in% colnames(x) && is.numeric(df)) {
            x[["df"]] <- df
        }
        
        if ("df" %in% colnames(x)) {
            x[["p.value"]] <- 2 * stats::pt(-abs(x$statistic), df = x[["df"]])

        # get_predicted does not save DF and does not compute p.value. We try
        # to extract df in predictions(), but this does not always work
        # (e.g., with hypothesis). When we don't have DF, normal p.value is misleading.
        } else if (!identical(vcov, "satterthwaite") || !identical(vcov, "kenward-roger")) {
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

    # option name change
    FUN_INTERVAL <- getOption("marginaleffects_posterior_interval")
    if (is.null(FUN_INTERVAL)) {
        FUN_INTERVAL <- getOption("marginaleffects_credible_interval", default = "eti")
    }
    checkmate::assert_choice(FUN_INTERVAL, choices = c("eti", "hdi"))
    if (FUN_INTERVAL == "hdi") {
        FUN_INTERVAL <- get_hdi
    } else {
        FUN_INTERVAL <- get_eti
    }

    FUN_CENTER <- getOption("marginaleffects_posterior_center", default = stats::median)
    checkmate::assert_function(FUN_CENTER)

    if (!"conf.low" %in% colnames(x) || isTRUE(overwrite)) {
        x[["std.error"]] <- NULL
        CIs <- t(apply(draws, 1, FUN_INTERVAL, credMass = conf_level))
        Bs <- apply(draws, 1, FUN_CENTER)
        # transform_pre returns a single value
        if (nrow(x) < nrow(CIs)) {
            CIs <- unique(CIs)
            Bs <- unique(Bs)
        }
        x[[estimate]] <- Bs
        x[["conf.low"]] <- CIs[, "lower"]
        x[["conf.high"]] <- CIs[, "upper"]
    }

    return(x)
}


get_eti <- function(object, credMass = 0.95, ...) {
  result <- c(NA_real_, NA_real_)
  checkmate::assert_numeric(object)
  checkmate::assert_numeric(credMass, len = 1)
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
