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
    # empty initial mfx data.frame means there were no numeric variables in the model
    if ("term" %in% colnames(x)) {
        dydx <- x[, colnames(x) %in% c("group", "term", "dydx", "std.error")]
        colnames(dydx)[match("dydx", colnames(dydx))] <- "estimate"

        if ("group" %in% colnames(dydx)) { 
            if ("std.error" %in% colnames(dydx)) {
                dydx <- poorman::group_by(dydx, group, term) 
                dydx <- poorman::summarize(dydx, poorman::across(c("estimate", "std.error"), mean, na.rm = TRUE))
            } else {
                dydx <- poorman::group_by(dydx, group, term)
                dydx <- poorman::summarize(dydx, estimate = mean, na.rm = TRUE)
            }
        } else {
            if ("std.error" %in% colnames(dydx)) {
                dydx <- poorman::group_by(dydx, term)
                dydx <- poorman::summarize(dydx, poorman::across(c("estimate", "std.error"), mean, na.rm = TRUE))
            } else {
                dydx <- poorman::group_by(dydx, term)
                dydx <- poorman::summarize(dydx, estimate = mean(estimate, na.rm = TRUE))
            }
        }
    } else {
        # avoids namespace conflict with `margins`
        dydx <- data.frame()
    }

    # dydx statistics (emmeans calculates those for us)
    if ("term" %in% colnames(dydx)) {
        if (!"statistic" %in% colnames(dydx)) dydx$statistic <- dydx$estimate / dydx$std.error
        if (!"p.value" %in% colnames(dydx)) dydx$p.value <- 2 * (1 - stats::pnorm(abs(dydx$statistic)))
    }

    # contrasts
    cont <- attr(x, "contrasts")
    if (nrow(dydx) > 0 && !is.null(cont)) {
        out <- poorman::bind_rows(dydx, attr(x, "contrasts"))
    } else if (nrow(dydx) > 0) {
        out <- dydx
    } else if (!is.null(cont)) {
        out <- cont
    } else {
        stop("tidy.marginaleffects could not extract marginal effects or contrasts from this object. Please file a bug report with a minimal reproducible example on Github.")
    }
        
    # confidence intervals
    if ("std.error" %in% colnames(out)) {
        if (isTRUE(conf.int) && !"conf.low" %in% colnames(out)) {
            alpha <- 1 - conf.level
            out$conf.low <- out$estimate + stats::qnorm(alpha / 2) * out$std.error
            out$conf.high <- out$estimate - stats::qnorm(alpha / 2) * out$std.error
        }
    }

    # sort and subset columns
    cols <- c("group", "term", "contrast", "estimate", "std.error",
              "statistic", "p.value", "conf.low", "conf.high")
    out <- out[, intersect(cols, colnames(out)), drop = FALSE]
    return(out)
}


#' @export
glance.marginaleffects <- function(x, ...) {
    out <- attr(x, "glance")
    return(out)
}
