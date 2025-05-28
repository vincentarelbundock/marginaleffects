#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.multinom_weightit <- function(model, ...) {
    stats::coef(model)
}
