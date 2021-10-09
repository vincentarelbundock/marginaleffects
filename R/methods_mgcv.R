#' @rdname get_coef
#' @export
get_coef.gam <- function(model, ...) {
    out <- model$coefficients
    return(out)
}
