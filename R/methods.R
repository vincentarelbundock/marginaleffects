#' @noRd
#' @export
vcov.comparisons <- function(object, ...) {
    attr(object, "jacobian") %*% attr(object, "vcov") %*% t(attr(object, "jacobian"))
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
coef.comparisons <- function(object, ...) {
  if (!is.null(object$estimate)) {
    return(object$estimate)
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