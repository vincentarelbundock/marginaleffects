#' @importFrom generics tidy
#' @export
generics::tidy



#' Tidy a `marginaleffects` object
#'
#' @param x An object produced by the `marginaleffects` function.
#' @inheritParams marginaleffects
#' @param conf_level numeric value between 0 and 1. Confidence level to use to build a confidence interval. The default `NULL` uses the `conf_level` value used in the original call to `marginaleffects()`.
#' @return A "tidy" `data.frame` of summary statistics which conforms to the
#' `broom` package specification.
#' @details
#' The `tidy` function calculates average marginal effects by taking the mean
#' of all the unit-level marginal effects computed by the `marginaleffects`
#' function.
#'
#' The standard error of the average marginal effects is obtained by 
#' taking the mean of each column of the Jacobian. . Then, we use this
#' "Jacobian at the mean" in the Delta method to obtained standard errors.
#'
#' In Bayesian models (e.g., `brms`), we compute Average Marginal
#' Effects by applying the mean function twice. First, we apply it to all
#' marginal effects for each posterior draw, thereby estimating one Average (or
#' Median) Marginal Effect per iteration of the MCMC chain. Second, we take
#' the `mean` and `quantile` function to the results of Step 1 to obtain the
#' Average (or Median) Marginal Effect and its associated interval.
#'
#' @family summary
#' @export
#' @examples
#' mod <- lm(mpg ~ hp * wt + factor(gear), data = mtcars)
#' mfx <- marginaleffects(mod)
#'
#' # average marginal effects
#' tidy(mfx)
tidy.marginaleffects <- function(x,
                                 conf_level = NULL,
                                 ...) {



    x_dt <- copy(x)
    setnames(x_dt, old = "dydx", new = "comparison")
    out <- tidy.comparisons(x_dt,
                            conf_level = conf_level,
                            ...)
    return(out)
}


#' Tidy a `deltamethod` object
#' @inheritParams tidy.marginaleffects
#' @family summary
#' @export
tidy.deltamethod <- function(x, ...) {
    if (any(!c("term", "estimate") %in% colnames(x)) || !inherits(x, c("deltamethod", "data.frame"))) {
        msg <- format_msg(
        "The `tidy()` method only supports `deltamethod` objects produced by the
        `marginaleffects::deltamethod()` function.")
        stop(msg, call. = FALSE)
    }
    # the object is already in a tidy format. We need this method for
    # `modelsummary` and other functions that rely on `tidy()`.
    return(x)
}


#' Tidy a `marginalmeans` object
#'
#' @param x An object produced by the `marginalmeans` function.
#' @inheritParams tidy.marginaleffects
#' @inheritParams tidy.comparisons
#' @return A "tidy" `data.frame` of summary statistics which conforms to the
#' `broom` package specification.
#' @family summary
#' @export
tidy.marginalmeans <- function(x,
                               conf_level = 0.95,
                               transform_avg = NULL,
                               ...) {


    transform_avg <- sanitize_transform_post(transform_avg)

    conf_level <- sanitize_conf_level(conf_level, ...)
    out <- x
    colnames(out)[colnames(out) == "marginalmean"] <- "estimate"

    draws <- attr(x, "posterior_draws")
    
    out <- get_ci(
        out,
        overwrite = FALSE,
        conf_level = conf_level,
        draws = draws,
        estimate = "estimate",
        ...)

    # back transformation
    if (!is.null(transform_avg)) {
        if (!is.null(attr(x, "transform_post"))) {
            msg <- "Estimates were transformed twice: once during the initial computation, and once more when summarizing the results in `tidy()` or `summary()`."
            warning(insight::format_message(msg), call. = FALSE)
        }
        out <- backtransform(out, transform_avg)
    }

    # sort and subset columns
    cols <- c("type", "term", "value", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    out <- out[, intersect(cols, colnames(out)), drop = FALSE]
    out <- as.data.frame(out)

    attr(out, "conf_level") <- conf_level
    attr(out, "nchains") <- attr(x, "nchains")
    attr(out, "transform_avg_label") <- names(transform_avg)[1]

    return(out)
}





