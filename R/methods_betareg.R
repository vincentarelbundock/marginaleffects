#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.betareg <- function(model, coefs) {
    # in betareg, coefficients are a two-element list. We want to substitute the first element!
    # vab: this must absolutely be a named vector, otherwise the precision gets mixed with coefs
    model[["coefficients"]]$mean[names(coefs)] <- coefs
    model
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.betareg <- function(model, ...) {
    model$coefficients$mean
}
