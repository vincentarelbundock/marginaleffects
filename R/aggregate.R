#' Marginalize Over Unit-Level Estimates
#' 
#' @export
aggregate.comparisons <- function(x, by = NULL, ...) {

    if (is.null(by)) {
        if (is.null(attr(x, "by"))) {
            by <- "type"
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
    idx <- colnames(out) %in% c("dydx", "predicted", "comparison")
    colnames(out)[idx] <- "estimate"
    return(out)
}

#' @rdname tidy.comparisons
#' @export
tidy.slopes <- tidy.comparisons

#' @rdname tidy.comparisons
#' @export
tidy.predictions <- tidy.comparisons