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