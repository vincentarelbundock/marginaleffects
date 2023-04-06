#' @noRd
#' @export
vcov.comparisons <- function(x) {
    attr(x, "jacobian") %*% attr(x, "vcov") %*% t(attr(x, "jacobian"))
}


#' @noRd
#' @export
vcov.predictions <- vcov.comparisons


#' @noRd
#' @export
vcov.slopes <- vcov.comparisons


#' @noRd
#' @export
vcov.marginalmeans <- vcov.comparisons


#' @export
#' @noRd
coef.comparisons <- function(x) {
  if (!is.null(x$estimate)) {
    return(x$estimate)
  } else {
    stop("The input object does not contain an 'estimate' element.")
  }
}


#' @export
#' @noRd
coef.slopes <- coef.comparisons


#' @export
#' @noRd
coef.marginalmeans <- coef.comparisons


#' @export
#' @noRd
coef.predictions <- coef.comparisons