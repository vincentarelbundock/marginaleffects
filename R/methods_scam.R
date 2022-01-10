#' @rdname set_coef
#' @export
set_coef.scam <- function(model, coefs) {
    # in basic model classes coefficients are named vector
    model[["coefficients.t"]][names(coefs)] <- coefs
    model
}


#' @rdname get_coef
#' @export
get_coef.scam <- function(model, ...) {
    model$coefficients.t
}


#' @rdname get_vcov
#' @export
get_vcov.scam <- function(model, ...) {
    V <- model$Vp.t
    b <- model$coefficients.t
    if (length(b) != nrow(V)) {
        stop("The size of the variance-covariance matrix does not match the length of the coefficients vector.")
    }
    colnames(V) <- row.names(V) <- names(b)
    return(V)
}
