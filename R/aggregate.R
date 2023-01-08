#' @importFrom stats aggregate
#' @export
stats::aggregate


#' Aggregate Estimates (aka "Marginalize", "Average Over", "Integrate")
#'
#' Calculate average estimates by taking the mean of all the
#' unit-level estimates computed by the `predictions`, `comparisons`, or `slopes` functions.
#'
#' @param x Object produced by the `predictions()`, `comparisons()`, or `slopes()` functions.
#' @param by Character vector of variable names over which to compute group-wise average estimates. When `by=NULL`, the global average (per term) is reported.
#' @inheritParams predictions
#' @param ... All additional arguments are passed to the original fitting
#' function to modify the original call options: `conf_level`, `transform_post`,
#' etc. See `?predictions`, `?comparisons`, `?slopes`.
#' @return A `data.frame` of estimates and uncertainty estimates
#' @details
#'
#' Standard errors are estimated using the delta method. See the `marginaleffects` website for details.
#'
#' In Bayesian models (e.g., `brms`), estimates are aggregated applying the
#' median (or mean) function twice. First, we apply it to all
#' marginal effects for each posterior draw, thereby estimating one Average (or
#' Median) Marginal Effect per iteration of the MCMC chain. Second, we
#' calculate the mean and the `quantile` function to the results of Step 1 to
#' obtain the Average Marginal Effect and its associated interval.
#'
#' @name aggregate
#' @examples
#' mod <- lm(mpg ~ factor(gear), data = mtcars)
#' contr <- comparisons(mod, variables = list(gear = "sequential"))
#' tidy(contr)
#' Marginalize Over Unit-Level Estimates
NULL


#' @rdname aggregate
#' @export
aggregate.predictions <- function(x, by = NULL, byfun = NULL, ...) {

    if (!is.null(byfun) && !inherits(x, "predictions")) {
        insight::format_error("The `byfun` argument is only supported for objects produced by the `predictions()` function.")
    }

    if (is.null(by)) {
        if (is.null(attr(x, "by"))) {
            by <- grep("^type$|^term$|^group$|^contrast_?", colnames(x), value = TRUE)
        } else {
            by <- attr(x, "by")
        }
    }

    # `bynout` requires us to re-eval a modified call
    out <- recall(x, by = by, byfun = byfun, ...)

    if (inherits(x, "predictions")) {
        data.table::setnames(out, "predicted", "estimate")
    } else if (inherits(x, "comparisons")) {
        data.table::setnames(out, "comparison", "estimate")
    } else if (inherits(x, "slopes")) {
        data.table::setnames(out, "dydx", "estimate")
    }

    # sort and subset columns
    cols <- c("type", "group", "term", "contrast",
              attr(x, "by"),
              grep("^contrast_\\w+", colnames(out), value = TRUE),
              "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    cols <- intersect(cols, colnames(out))

    # hack to select columns while preserving attributes
    for (v in colnames(out)) {
        if (!v %in% cols) {
            out[[v]] <- NULL
        }
    }
    data.table::setDF(out)

    return(out)
}


#' @rdname aggregate
#' @export
aggregate.comparisons <- function(x, by = NULL, ...) {

    if (!is.null(byfun) && !inherits(x, "predictions")) {
        insight::format_error("The `byfun` argument is only supported for objects produced by the `predictions()` function.")
    }

    if (is.null(by)) {
        if (is.null(attr(x, "by"))) {
            by <- grep("^type$|^term$|^group$|^contrast_?", colnames(x), value = TRUE)
        } else {
            by <- attr(x, "by")
        }
    }

    # `bynout` requires us to re-eval a modified call
    out <- recall(x, by = by, ...)

    if (inherits(x, "predictions")) {
        data.table::setnames(out, "predicted", "estimate")
    } else if (inherits(x, "comparisons")) {
        data.table::setnames(out, "comparison", "estimate")
    } else if (inherits(x, "slopes")) {
        data.table::setnames(out, "dydx", "estimate")
    }

    # sort and subset columns
    cols <- c("type", "group", "term", "contrast",
              attr(x, "by"),
              grep("^contrast_\\w+", colnames(out), value = TRUE),
              "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    cols <- intersect(cols, colnames(out))

    # hack to select columns while preserving attributes
    for (v in colnames(out)) {
        if (!v %in% cols) {
            out[[v]] <- NULL
        }
    }
    data.table::setDF(out)

    return(out)
}


#' @rdname aggregate
#' @export
aggregate.slopes <- aggregate.comparisons

