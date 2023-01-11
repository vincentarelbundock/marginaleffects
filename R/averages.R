#' Average Estimates (aka "Margins")
#'
#' @description
#' Calculate average estimates by taking the (group-wise) mean of all the unit-level
#' estimates computed by the `predictions()`, `comparisons()`, or `slopes()` functions.
#' 
#' Warning: It is generally faster and safer to use the `by` argument of one of
#' the three functions listed above. Alternatively, one can call it in one step:
#' 
#' `average(slopes(model))`
#' 
#' `slopes(model) |> averages()` 
#' 
#' Proceeding in two steps by assigning the unit-level estimates is typically
#' slower, because the whole estimate must be executed twice.
#'
#' Note that the `tidy()` and `summary()` methods are simple wrappers around `averages()`
#' @param x Object produced by the `predictions()`, `comparisons()`, or `slopes()` functions.
#' @param by Character vector of variable names over which to compute group-wise average estimates. When `by=NULL`, the global average (per term) is reported.
#' @inheritParams predictions
#' @param ... All additional arguments are passed to the original fitting
#' function to modify the original call options: `conf_level`, `transform_post`,
#' etc. See `?predictions`, `?comparisons`, `?slopes`.
#' @export
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
#' @examples
#' mod <- lm(mpg ~ factor(gear), data = mtcars)
#' contr <- comparisons(mod, variables = list(gear = "sequential"))
#' tidy(contr)
averages <- function (x, by = NULL, ...) {
    xcall <- substitute(x)
    if (is.call(xcall)) {
        if (is.null(by)) {
            by <- c("term", "group", "contrast")
        }
        out <- recall(xcall, by = by, ...)
        return(out)
    }

    UseMethod("averages", x)
}


#' @rdname averages
#' @export
averages.predictions <- function(x, by = NULL, byfun = NULL, ...) {

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
              "by",
              grep("^contrast_\\w+", colnames(out), value = TRUE),
              "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    if (isTRUE(checkmate::check_character(by))) {
        cols <- c(cols, by)
    }
    cols <- intersect(cols, colnames(out))

    # hack to select columns while preserving attributes
    for (v in colnames(out)) {
        if (!v %in% cols) {
            out[[v]] <- NULL
        }
    }
    data.table::setDF(out)

    attr(out, "averages") <- TRUE

    return(out)
}


#' @rdname averages
#' @export
averages.comparisons <- function(x, by = NULL, ...) {

    if ("byfun" %in% names(list(...)) && !inherits(x, "predictions")) {
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
              "by",
              attr(x, "by"),
              grep("^contrast_\\w+", colnames(out), value = TRUE),
              "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    if (isTRUE(checkmate::check_character(by))) {
        cols <- c(cols, by)
    }
    cols <- intersect(cols, colnames(out))

    # hack to select columns while preserving attributes
    for (v in colnames(out)) {
        if (!v %in% cols) {
            out[[v]] <- NULL
        }
    }
    data.table::setDF(out)

    attr(out, "averages") <- TRUE

    return(out)
}


#' @rdname averages
#' @export
averages.slopes <- averages.comparisons