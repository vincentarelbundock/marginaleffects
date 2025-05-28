#' @rdname get_coef
#' @export
get_coef.systemfit <- function(model, ...) {
    out <- stats::coef(model)
    return(out)
}

#' @rdname get_vcov
#' @export
get_vcov.systemfit <- function(model, ...) {
    vcov <- sanitize_vcov(model, vcov)
    out <- stats::vcov(model)
    return(out)
}
