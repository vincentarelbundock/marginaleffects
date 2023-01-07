#' @importFrom generics tidy
#' @export
generics::tidy

#' @noRd
#' @export
tidy.comparisons <- function(x, ...) {
    out <- aggregate(x, ...)
    if (inherits(x, c("comparisons", "slopes", "marginalmeans"))) {
        idx <- colnames(out) %in% c("dydx", "comparison", "marginalmeans")
        colnames(out)[idx] <- "estimate"
    } else if (inherits(x, "predictions")) {
        idx <- colnames(out) %in% "predicted"
        colnames(out)[idx] <- "estimate"
    }
    return(out)
}

#' @noRd
#' @export
tidy.slopes <- tidy.comparisons

#' @rdname tidy.comparisons
#' @export
tidy.predictions <- tidy.comparisons

#' @noRd
#' @export
tidy.hypotheses <- function(x, ...) {
    if (any(!c("term", "estimate") %in% colnames(x)) || !inherits(x, c("hypotheses", "deltamethod", "data.frame"))) {
        insight::format_error("The `tidy()` method only supports `hypotheses` objects produced by the `marginaleffects::hypotheses()` function.")
    }
    # the object is already in a tidy format. We need this method for
    # `modelsummary` and other functions that rely on `tidy()`.
    return(x)
}

#' Tidy a `marginalmeans` object
#'
#' @param x An object produced by the `marginalmeans` function.
#' @inheritParams tidy.slopes
#' @inheritParams tidy.comparisons
#' @return A "tidy" `data.frame` of summary statistics which conforms to the
#' `broom` package specification.
#' @template bayesian
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