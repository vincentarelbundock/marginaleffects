#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.mlm <- function(model, coefs, ...) {
    model$coefficients[] <- coefs

    return(model)
}

#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.mlm <- function(model, ...) {
    out <- insight::get_parameters(model, ...)
    out <- stats::setNames(
        out$Estimate,
        sprintf("%s:%s", out$Response, out$Parameter)
    )
    return(out)
}


#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.mlm <- function(model, ...) {
    resp <- insight::get_response(model)
    return(names(resp))
}
