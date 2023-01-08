#' @importFrom stats aggregate
#' @export
stats::aggregate


#' Aggregate (marginalize, integrate, average over) a `comparisons` object
#'
#' Calculate average contrasts by taking the mean of all the
#' unit-level contrasts computed by the `predictions` function.
#'
#' @param x An object produced by the `comparisons` function.
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
#' @name aggregate
#' @examples
#' mod <- lm(mpg ~ factor(gear), data = mtcars)
#' contr <- comparisons(mod, variables = list(gear = "sequential"))
#' tidy(contr)
#' Marginalize Over Unit-Level Estimates
NULL


#' @rdname aggregate
#' @export
aggregate.comparisons <- function(x, by = NULL, ...) {

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


#' @rdname aggregate
#' @export
aggregate.predictions <- aggregate.comparisons


