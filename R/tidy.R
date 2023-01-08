#' @importFrom generics tidy
#' @noRd
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


#' @noRd
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


#' @noRd
#' @export
tidy.marginalmeans <- function(x, ...) {
    colnames(x)[colnames(x) == "marginalmean"] <- "estimate"
    return(x)
}