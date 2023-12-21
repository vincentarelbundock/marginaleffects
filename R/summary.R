#' @noRd
#' @export
summary.slopes <- function(object, ...) {
    .Deprecated("`avg_slopes()` or the `by` argument")
    out <- get_averages(object, ...)
    return(out)
}


#' @noRd
#' @export
summary.predictions <- function(...) {
    .Deprecated("`avg_predictions()` or the `by` argument")
    out <- get_averages(object, ...)
    return(out)
}


#' @noRd
#' @export
summary.comparisons <- function(...) {
    .Deprecated("`avg_comparisons()` or the `by` argument")
    out <- get_averages(object, ...)
    return(out)
}


#' @noRd
#' @export
summary.marginalmeans <- function(object, ...) {
    return(object)
}


#' @noRd
#' @export
summary.hypotheses <- function(object, ...) {
    return(object)
}