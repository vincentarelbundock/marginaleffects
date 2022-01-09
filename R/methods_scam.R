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
    ## faster
    # out <-  stats::coef(model)

    # more general
    out <- insight::get_parameters(model)
    out <- stats::setNames(out$Estimate, out$Parameter)
    return(out)
}
