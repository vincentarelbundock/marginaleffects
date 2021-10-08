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
#' @return A "tidy" `data.frame` of summary statistics which conforms to the
#' `broom` package specification.
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

    # dydx averages
    # empty initial mfx data.frame means there were no numeric variables in the
    # model
    if ("term" %in% colnames(x)) {
        cols <- c("type", "group", "term")
        cols <- intersect(cols, colnames(x))
        cols <- paste(cols, collapse = " + ")
        f <- stats::as.formula(sprintf("dydx ~ %s", cols))

        dydx <- stats::aggregate(f, data = x, FUN = mean)

        ## This might be a useful implementation of weights
        # if (is.null(attr(x, "weights"))) {
        #     dydx <- stats::aggregate(f, data = x, FUN = mean)
        # } else {
        #     dydx <- stats::aggregate(f, data = x, FUN = weighted.mean, w = attr(x, "weights"))
        # }

        se <- attr(x, "se_at_mean_gradient")
        if (!is.null(se)) {
            dydx <- merge(dydx, se, all.x = TRUE)
        }
        colnames(dydx)[match("dydx", colnames(dydx))] <- "estimate"
    } else {
        # avoids namespace conflict with `margins`
        dydx <- data.frame()
    }

    if (!"statistic" %in% colnames(dydx) && "std.error" %in% colnames(dydx)) {
        dydx$statistic <- dydx$estimate / dydx$std.error
    }

    if (!"p.value" %in% colnames(dydx) && "std.error" %in% colnames(dydx)) {
        dydx$p.value <- 2 * (1 - stats::pnorm(abs(dydx$statistic)))
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

    attr(out, "conf.level") <- conf.level

    return(out)
}


#' @export
glance.marginaleffects <- function(x, ...) {
    out <- attr(x, "glance")
    return(out)
}


#' Tidy a `marginalmeans` object
#'
#' @param x An object produced by the `marginalmeans` function.
#' @inheritParams tidy.marginaleffects
#' @return A "tidy" `data.frame` of summary statistics which conforms to the
#' `broom` package specification.
#' @export
tidy.marginalmeans <- function(x,
                                 conf.int = TRUE,
                                 conf.level = 0.95,
                                 ...) {

    out <- x
    colnames(out)[colnames(out) == "value"] <- "group"
    colnames(out)[colnames(out) == "predicted"] <- "estimate"

    if (!"statistic" %in% colnames(out) && "std.error" %in% colnames(out)) {
        out$statistic <- out$estimate / out$std.error
    }

    if (!"p.value" %in% colnames(out) && "std.error" %in% colnames(out)) {
        out$p.value <- 2 * (1 - stats::pnorm(abs(out$statistic)))
    }

    out <- out

    # confidence intervals
    if ("std.error" %in% colnames(out)) {
        if (isTRUE(conf.int) && !"conf.low" %in% colnames(out)) {
            alpha <- 1 - conf.level
            out$conf.low <- out$estimate + stats::qnorm(alpha / 2) * out$std.error
            out$conf.high <- out$estimate - stats::qnorm(alpha / 2) * out$std.error
        }
    }

    # sort and subset columns
    cols <- c("type", "term", "group", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    out <- out[, intersect(cols, colnames(out)), drop = FALSE]
    out <- as.data.frame(out)

    attr(out, "conf.level") <- conf.level

    return(out)
}


#' @export
glance.marginalmeans <- function(x, ...) {
    out <- attr(x, "glance")
    return(out)
}
