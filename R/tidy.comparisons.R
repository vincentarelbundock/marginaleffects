#' Tidy a `comparisons` object
#'
#' Calculate average contrasts by taking the mean of all the
#' unit-level contrasts computed by the `predictions` function.
#'
#' @param x An object produced by the `comparisons` function.
#' @param transform_avg A function applied to the estimates and confidence intervals *after* the unit-level estimates have been averaged.
#' @param conf_level numeric value between 0 and 1. Confidence level to use to build a confidence interval. The default `NULL` uses the `conf_level` value used in the original call to `comparisons()`.
#' @inheritParams comparisons
#' @inheritParams tidy.marginaleffects
#' @return A "tidy" `data.frame` of summary statistics which conforms to the
#' `broom` package specification.
#' @details
#'
#' To compute standard errors around the average marginaleffects, we begin by applying the
#' mean function to each column of the Jacobian. Then, we use this matrix in the Delta
#' method to obtained standard errors.
#'
#' In Bayesian models (e.g., `brms`), we compute Average Marginal
#' Effects by applying the mean function twice. First, we apply it to all
#' marginal effects for each posterior draw, thereby estimating one Average (or
#' Median) Marginal Effect per iteration of the MCMC chain. Second, we
#' calculate the mean and the `quantile` function to the results of Step 1 to
#' obtain the Average Marginal Effect and its associated interval.
#'
#' @family summary
#' @export
#' @examples
#' mod <- lm(mpg ~ factor(gear), data = mtcars)
#' contr <- comparisons(mod, variables = list(gear = "sequential"))
#' tidy(contr)
tidy.comparisons <- function(x,
                             conf_level = NULL,
                             transform_avg = NULL,
                             ...) {

    # use original conf_level by default
    if (is.null(conf_level)) {
        conf_level <- attr(x, "conf_level")
    }

    if (identical(attr(x, "transform_pre"), "lnor")) {
        msg <- 
        'The `tidy()` and `summary()` functions take the average of estimates
        over the whole dataset. However, the unit-level estimates you requested 
        are not collapsible. Please use `transform_pre="lnoravg"` instead.' 
        stop(msg, call. = FALSE)
    }

    if ("by" %in% names(list(...))) {
        msg <- 
        "The `by` argument is deprecated in this function. You can use `by` in the `comparisons()`, 
        `marginaleffects()`, and `predictions()` functions instead."
        stop(msg, call. = FALSE)
    }

    dots <- list(...)
    conf_level <- sanitize_conf_level(conf_level, ...)
    checkmate::assert_function(transform_avg, null.ok = TRUE)

    transform_avg <- deprecation_arg(
        transform_avg,
        newname = "transform_avg",
        oldname = "transform_post",
        ...)

    x_dt <- data.table(x)

    marginaleffects_wts_internal <- attr(x, "weights")

    draws <- attr(x, "posterior_draws")

    idx_by <- c("type", "group", "term", "contrast", 
                grep("^contrast_\\w+", colnames(x_dt), value = TRUE))
    idx_by <- intersect(idx_by, colnames(x_dt))
    idx_na <- is.na(x_dt$comparison)

    # do not use the standard errors if we already have the final number of rows (e.g., lnoravg)
    flag_delta <- nrow(unique(x_dt[, ..idx_by])) != nrow(x_dt)

    if (!is.null(marginaleffects_wts_internal)) {
        x_dt[, "marginaleffects_wts_internal" := marginaleffects_wts_internal]
    }

    # bayesian
    if (!is.null(draws)) {
        ame <- average_draws(
            data = x_dt,
            index = idx_by,
            draws = draws,
            column = "comparisons")
        draws <- attr(ame, "posterior_draws")

    # frequentist
    # empty initial mfx data.frame means there were no numeric variables in the
    # model
    } else if (isTRUE(flag_delta) && is.null(attr(x, "by")) && ("term" %in% colnames(x_dt) || inherits(x, "predictions"))) {

        J <- attr(x, "jacobian")
        V <- attr(x, "vcov")

        # average marginal effects
        if ("marginaleffects_wts_internal" %in% colnames(x_dt)) {
            ame <- x_dt[idx_na == FALSE,
                        .(estimate = stats::weighted.mean(
                            comparison,
                            marginaleffects_wts_internal,
                            na.rm = TRUE)),
                        by = idx_by]
        } else {
            ame <- x_dt[idx_na == FALSE,
                        .(estimate = mean(comparison, na.rm = TRUE)),
                        by = idx_by]
        }

        if (is.matrix(J) && is.matrix(V)) {
            # Jacobian at the group mean
            # use weird colnames to avoid collision
            idx_pad <- x_dt[, ..idx_by]
            idx_col_old <- colnames(idx_pad)
            idx_col_new <- paste0(idx_col_old, "_marginaleffects_index")
            setnames(idx_pad,
                     old = colnames(idx_pad),
                     new = paste0(colnames(idx_pad), "_marginaleffects_index"))

            J <- data.table(idx_pad, J)

            J <- J[idx_na == FALSE, ]
            x_dt <- x_dt[idx_na == FALSE, ]

            tmp <- paste0(idx_by, "_marginaleffects_index")

            if (is.null(marginaleffects_wts_internal)) {
                J_mean <- J[, lapply(.SD, mean, na.rm = TRUE), by = tmp]
            } else {
                J[, "marginaleffects_wts_internal" := marginaleffects_wts_internal]
                J_mean <- J[,
                lapply(.SD,
                        stats::weighted.mean,
                        w = marginaleffects_wts_internal,
                        na.rm = TRUE),
                by = tmp]
            }
            if ("marginaleffects_wts_internal" %in% colnames(J_mean)) {
                tmp <- c("marginaleffects_wts_internal", tmp)
            }
            J_mean <- J_mean[, !..tmp]
            J_mean <- as.matrix(J_mean)

            # HACK: align J_mean and V if they don't match
            if (all(colnames(J_mean) %in% colnames(V))) {
                V <- V[colnames(J_mean), colnames(J_mean)]
            }

            # standard errors at the group mean
            se <- sqrt(colSums(t(J_mean %*% V) * t(J_mean)))
            ame[, std.error := se]
        }

    } else {
        # avoids namespace conflict with `margins`
        ame <- x_dt
        setnames(ame, old = "comparison", new = "estimate")
    }

    out <- get_ci(
        ame,
        overwrite = FALSE,
        conf_level = conf_level,
        draws = draws,
        estimate = "estimate")

    # remove terms with precise zero estimates. typically the case in
    # multi-equation models where some terms only affect one response
    out <- out[out$estimate != 0, ]

    # back transformation
    if (!is.null(transform_avg)) {
        if (!is.null(attr(x, "transform_post"))) {
            msg <- "Estimates were transformed twice: once during the initial computation, and once more when summarizing the results in `tidy()` or `summary()`."
            warning(insight::format_message(msg), call. = FALSE)
        }
        out <- backtransform(out, transform_avg)
    }

    # sort and subset columns
    cols <- c("type", "group", "term", "contrast",
              attr(x, "by"),
              grep("^contrast_\\w+", colnames(x_dt), value = TRUE),
              "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    cols <- intersect(cols, colnames(out))
    out <- out[, cols, drop = FALSE, with = FALSE]


    setDF(out)

    attr(out, "conf_level") <- conf_level
    attr(out, "FUN") <- "mean"

    if (exists("drawavg")) {
        class(drawavg) <- c("posterior_draws", class(drawavg))
        attr(out, "posterior_draws") <- drawavg
    }

    if (exists("J_mean")) {
        attr(out, "jacobian") <- J_mean
    }


    return(out)
}
