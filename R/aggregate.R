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

    # `by` requires us to re-eval a modified call
    out <- recall(x, by = by, ...)

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