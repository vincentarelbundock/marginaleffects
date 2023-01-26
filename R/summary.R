#' @noRd
#' @export
summary.slopes <- function(object, ...) {
    if (isFALSE(attr(object, "by"))) {
        out <- averages(object, ...)
    } else {
        out <- object
    }
    return(out)
}


#' @noRd
#' @export
summary.predictions <- summary.slopes


#' @noRd
#' @export
summary.predictions <- summary.slopes


#' @noRd
#' @export
summary.marginalmeans <- function(object, ...) {
    return(object)
}


#' @noRd
#' @export
summary.hypotheses <- summary.marginalmeans