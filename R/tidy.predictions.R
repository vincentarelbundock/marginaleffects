#' Tidy a `predictions` object
#'
#' Calculate average adjusted predictions by taking the mean of all the
#' unit-level adjusted predictions computed by the `predictions` function.
#'
#' @param x An object produced by the `predictions` function.
#' @inheritParams predictions
#' @inheritParams tidy.comparisons
#' @param conf_level numeric value between 0 and 1. Confidence level to use to build a confidence interval. The default `NULL` uses the `conf_level` value used in the original call to `predictions()`.
#' @return A "tidy" `data.frame` of summary statistics which conforms to the
#' `broom` package specification.
#' @family summary
#' @export
#' @examples
#' mod <- lm(mpg ~ hp * wt + factor(gear), data = mtcars)
#' mfx <- predictions(mod)
#' tidy(mfx)
tidy.predictions <- function(x,
                             conf_level = NULL,
                             transform_avg = NULL,
                             ...) {

    checkmate::assert_function(transform_avg, null.ok = TRUE)

    # use original conf_level by default
    if (is.null(conf_level)) {
        conf_level <- attr(x, "conf_level")
    }

    # I left the `by` code below in case I eventually want to revert. Much
    # of it needs to stay anyway because we need the `delta` in `tidy` for
    # average predicted values, but Isome stuff could eventually be cleaned up.
    if ("by" %in% names(list(...))) {
        msg <- 
        "The `by` argument is deprecated in this function. You can use `by` in the `comparisons()`, 
        `marginaleffects()`, and `predictions()` functions instead."
        stop(msg, call. = FALSE)
    }

    x_dt <- copy(data.table(x))

    marginaleffects_wts_internal <- attr(x, "weights")

    if ("group" %in% colnames(x_dt)) {
        idx <- "group"
    } else {
        idx <- NULL
    }

    fun <- function(...) {
        dots <- list(...)
        dots[["eps"]] <- NULL
        dots[["vcov"]] <- FALSE
        out <- data.table(do.call("predictions", dots))
        if (!is.null(marginaleffects_wts_internal)) {
            out[, "marginaleffects_wts_internal" := marginaleffects_wts_internal]
            out <- out[,
                .(predicted = stats::weighted.mean(predicted, marginaleffects_wts_internal, na.rm = TRUE)),
                by = idx]
        } else {
            out <- out[, .(predicted = mean(predicted)), by = idx]
        }
        return(out$predicted)
    }

    # only aggregate if predictions were not already aggregated before
    if (is.null(attr(x, "by"))) {

        # bayesian
        draws <- attr(x, "posterior_draws")
        if (!is.null(draws)) {
            bycols <- NULL
            x_dt <- average_draws(
                data = x_dt,
                index = bycols,
                draws = draws,
                column = "predicted")

        # frequentist
        } else {
            if (!is.null(marginaleffects_wts_internal)) {
                x_dt[, "marginaleffects_wts_internal" := marginaleffects_wts_internal]
                x_dt <- x_dt[,
                .(predicted = stats::weighted.mean(
                    predicted,
                    marginaleffects_wts_internal,
                    na.rm = TRUE)),
                by = idx]
            } else {
                x_dt <- x_dt[, .(predicted = mean(predicted)), by = idx]
            }
        }
    }

    if (!"std.error" %in% colnames(x_dt)) {
        se <- get_se_delta(
            model = attr(x, "model"),
            newdata = attr(x, "newdata"),
            vcov = attr(x, "vcov"),
            type = attr(x, "type"),
            FUN = fun,
            ...)
        if (!is.null(se)) {
            x_dt[, "std.error" := se]
        }
    }

    setnames(x_dt, old = "predicted", new = "estimate")

    # confidence intervals
    out <- get_ci(
        x_dt,
        estimate = "estimate",
        conf_level = conf_level,
        draws = attr(x_dt, "posterior_draws"))

    # back transformation
    if (!is.null(transform_avg)) {
        if (!is.null(attr(x, "transform_post"))) {
            msg <- "Estimates were transformed twice: once during the initial computation, and once more when summarizing the results in `tidy()` or `summary()`."
            warning(insight::format_message(msg), call. = FALSE)
        }
        out <- backtransform(out, transform_avg)
    }

    return(out)
}
