#' @importFrom generics tidy
#' @export
generics::tidy


#' @importFrom generics glance
#' @export
generics::glance


#' Tidy a `marginaleffects` object
#'
#' @param x An object produced by the `marginaleffects` function.
#' @param conf.int Logical indicating whether or not to include a confidence interval.
#' @param conf.level The confidence level to use for the confidence interval if
#'   `conf.int=TRUE`. Must be strictly greater than 0 and less than 1. Defaults
#'   to 0.95, which corresponds to a 95 percent confidence interval.
#' @inheritParams marginaleffects
#' @details
#' The `tidy` function calculates average marginal effects by taking the mean
#' of all the unit-level marginal effects computed by the `marginaleffects`
#' function.
#' @export
#' @examples
#' mod <- lm(mpg ~ hp * wt + factor(gear), data = mtcars)
#' mfx <- marginaleffects(mod)
#' tidy(mfx)
tidy.marginaleffects <- function(x,
                                 conf.int = TRUE,
                                 conf.level = 0.95,
                                 ...) {

    if (length(attr(x, "contrasts")) > 1) {
        if (isFALSE(check_dependency("emmeans"))) {
            warning("The model includes logical and/or factor variables. You can install the `emmeans` package to compute contrasts.")
        }
    }


    # dydx averages 
    # empty initial mfx data.frame means there were no numeric variables in the
    # model
    if ("term" %in% colnames(x)) {
        cols <- c("type", "group", "term")
        cols <- intersect(cols, colnames(x))
        cols <- paste(cols, collapse = " + ")
        f <- as.formula(sprintf("dydx ~ %s", cols))
        dydx <- aggregate(f, data = x, FUN = mean) |> head()
        dydx <- merge(dydx, attr(x, "se_at_mean_gradient"))
        colnames(dydx)[match("dydx", colnames(dydx))] <- "estimate"
    } else {
        # avoids namespace conflict with `margins`
        dydx <- data.frame()
    }

    # dydx statistics (emmeans calculates those for us)
    if ("term" %in% colnames(dydx)) {
        if (!"statistic" %in% colnames(dydx)) {
            dydx$statistic <- dydx$estimate / dydx$std.error
        }
        if (!"p.value" %in% colnames(dydx)) {
            dydx$p.value <- 2 * (1 - stats::pnorm(abs(dydx$statistic)))
        }
    }

    out <- dydx

    # confidence intervals
    if ("std.error" %in% colnames(out)) {
        if (isTRUE(conf.int) && !"conf.low" %in% colnames(out)) {
            alpha <- 1 - conf.level
            out$conf.low <- out$estimate + stats::qnorm(alpha / 2) * out$std.error
            out$conf.high <- out$estimate - stats::qnorm(alpha / 2) * out$std.error
        }
    }

    # remove terms with precise zero estimates. typically the case in
    # multi-equation models where some terms only affect one response
    out <- out[out$estimate != 0,]

    # sort and subset columns
    cols <- c("type", "group", "term", "contrast", "estimate", "std.error",
              "statistic", "p.value", "conf.low", "conf.high")
    out <- out[, intersect(cols, colnames(out)), drop = FALSE]
    out <- as.data.frame(out)
    return(out)
}


#' @export
glance.marginaleffects <- function(x, ...) {
    out <- attr(x, "glance")
    return(out)
}
