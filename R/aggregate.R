#' Marginalize Over Unit-Level Estimates
#' 
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
    data.table::setDF(out)
    out <- as.data.frame(out)[, cols, drop = FALSE]

    return(out)
}

#' @export
aggregate.slopes <- aggregate.comparisons

#' @export
aggregate.predictions <- aggregate.comparisons


#' Blah
#' 
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

#' @rdname tidy.comparisons
#' @export
tidy.slopes <- tidy.comparisons

#' @rdname tidy.comparisons
#' @export
tidy.predictions <- tidy.comparisons