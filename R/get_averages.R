#' Average Estimates (aka "Margins")
#'
#' @description
#' Calculate average estimates by taking the (group-wise) mean of all the unit-level
#' estimates computed by the `predictions()`, `comparisons()`, or `slopes()` functions.
#' 
#' Warning: It is generally faster and safer to use the `by` argument of one of
#' the three functions listed above. Alternatively, one can call it in one step:
#' 
#' `avg_slopes(model)`
#' 
#' `slopes(model, by = TRUE)`
#' 
#' Proceeding in two steps by assigning the unit-level estimates is typically
#' slower, because all estimates must be computed twice.
#'
#' Note that the `tidy()` and `summary()` methods are slower wrappers around `avg_*()` functions.
#' @param x Object produced by the `predictions()`, `comparisons()`, or `slopes()` functions.
#' @param by Character vector of variable names over which to compute group-wise average estimates. When `by=NULL`, the global average (per term) is reported.
#' @param ... All additional arguments are passed to the original fitting
#' function to override the original call options: `conf_level`, `transform`,
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
#' @keywords internal
#' @examplesIf interactive() || isTRUE(Sys.getenv("R_DOC_BUILD") == "true")
#' @examples
#' mod <- lm(mpg ~ factor(gear), data = mtcars)
#' avg_comparisons(mod, variables = list(gear = "sequential"))
#' 
get_averages <- function (x, by = TRUE, ...) {
    xcall <- substitute(x)
    if (is.call(xcall)) {
        if ("by" %in% names(xcall)) {
            if (!isTRUE(checkmate::check_flag(by, null.ok = TRUE))) {
                insight::format_error("The `by` argument cannot be used twice.")
            }
            if (length(list(...)) == 0) { # bug in predictions.Rmd 
                out <- eval(xcall)
            } else {
                out <- recall(xcall, ...)
            }
        } else if (isTRUE(checkmate::check_flag(by, null.ok = TRUE))) {
            by <- c("term", "group", "contrast")
            out <- recall(xcall, by = by, ...)
        } else {
            out <- recall(xcall, by = by, ...)
        }
        return(out)
    }

    UseMethod("get_averages", x)
}


#' @noRd
get_averages.predictions <- function(x, by = TRUE, byfun = NULL, ...) {

    if (!is.null(byfun) && !inherits(x, "predictions")) {
        insight::format_error("The `byfun` argument is only supported for objects produced by the `predictions()` function.")
    }

    if (!isFALSE(attr(x, "by")) && !is.null(attr(x, "by"))) {
        return(x)
    }

    if (is.null(by) || isFALSE(by)) {
        by <- grep("^type$|^term$|^group$|^contrast_?", colnames(x), value = TRUE)
    }

    # `bynout` requires us to re-eval a modified call
    out <- recall(x, by = by, byfun = byfun, ...)

    # sort and subset columns
    cols <- c("group", "term", "contrast",
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

    return(out)
}


#' @noRd
get_averages.comparisons <- function(x, by = TRUE, ...) {

    if ("byfun" %in% names(list(...)) && !inherits(x, "predictions")) {
        insight::format_error("The `byfun` argument is only supported for objects produced by the `predictions()` function.")
    }

    # already used `by` in the main call, so we return the main output
    if (!isFALSE(attr(x, "by")) && !is.null(attr(x, "by"))) {
        return(x)
    }

    if (isTRUE(checkmate::check_flag(by, null.ok = TRUE))) {
        by <- grep("^type$|^term$|^group$|^contrast_?", colnames(x), value = TRUE)
    }

    # `bynout` requires us to re-eval a modified call
    out <- recall(x, by = by, ...)

    # sort and subset columns
    cols <- c("group", "term", "contrast",
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

    return(out)
}


#' @noRd
get_averages.slopes <- get_averages.comparisons





#' @noRd
get_averages.hypotheses <- get_averages.marginalmeans