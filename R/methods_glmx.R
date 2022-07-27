#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.glmx <- function(model, coefs, ...) {
    out <- model
    out$coefficients$glm <- coefs[names(out$coefficients$glm)]
    out$coefficients$extra <- coefs[names(out$coefficients$extra)]
    return(out)
}
