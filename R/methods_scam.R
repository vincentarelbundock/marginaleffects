#' @rdname set_coef
#' @export
set_coef.scam <- function(model, coefs, ...) {
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
get_vcov.scam <- function(model, vcov = NULL, ...) {
    if (isTRUE(checkmate::check_matrix(vcov))) {
        return(vcov)
    }

    vcov <- sanitize_vcov(model, vcov)

    # email from developer Natalya Pya
    # "one of the elements of the returned 'scam' object is 'Vp.t' which is an
    # estimated covariance matrix for the reparametrized parameters,
    # 'model$coefficients.t'."

    if (!is.null(vcov) && !is.logical(vcov)) {
        stop(
            "The `vcov` argument is not supported for models of class `scam`.",
            .call = FALSE
        )
    }

    V <- model$Vp.t
    b <- model$coefficients.t
    if (length(b) != nrow(V)) {
        stop(
            "The size of the variance-covariance matrix does not match the length of the coefficients vector.",
            call. = FALSE
        )
    }
    colnames(V) <- row.names(V) <- names(b)
    return(V)
}
