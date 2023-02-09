#' @noRd
#' @export
summary.slopes <- function(object, ...) {
    out <- get_averages(object, ...)
    return(out)
}


#' @noRd
#' @export
summary.predictions <- summary.slopes


#' @noRd
#' @export
summary.comparisons <- summary.slopes


#' @noRd
#' @export
summary.marginalmeans <- function(object, ...) {
    return(object)
}


#' @noRd
#' @export
summary.hypotheses <- summary.marginalmeans