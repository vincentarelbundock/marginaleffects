#' @rdname set_coef
#' @export
set_coef.hetprob <- function(model, coefs, ...) {
    model[["estimate"]][names(coefs)] <- coefs
    model
}

#' @rdname set_coef
#' @export
set_coef.ivpml <- set_coef.hetprob
