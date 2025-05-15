#' @rdname set_coef
#' @export
set_coef.lme <- function(model, coefs, ...) {
    model[["coefficients"]][["fixed"]][names(coefs)] <- coefs
    model
}
