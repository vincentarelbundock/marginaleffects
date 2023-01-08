#' Get a named vector of coefficients from a model object (internal function)
#' 
#' @inheritParams slopes
#' @return A named vector of coefficients. The names must match those of the variance matrix.
#' @rdname get_coef
#' @keywords internal
#' @export
get_coef <- function (model, ...) {
    UseMethod("get_coef", model)
}

#' @rdname get_coef
#' @export
get_coef.default <- function(model, ...) {
    ## faster
    # out <-  stats::coef(model)

    # more general
    out <- insight::get_parameters(model, component = "all")
    out <- stats::setNames(out$Estimate, out$Parameter)
    return(out)
}
