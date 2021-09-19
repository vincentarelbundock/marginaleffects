#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.polr <- function(model, ...) {
    out <- insight::get_parameters(model)
    out <- stats::setNames(out$Estimate, out$Parameter)
    names(out) <- gsub("Intercept: ", "", names(out))
    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.polr <- function(model, coefs) {
    # in basic model classes coefficients are named vector
    idx <- match(names(model$coefficients), names(coefs))
    model[["coefficients"]] <- coefs[idx]
    idx <- match(names(model$zeta), names(coefs))
    model[["zeta"]] <- coefs[idx]
    model
}


#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.polr <- function(model, ...) {
    out <- suppressMessages(insight::get_varcov(model))
    return(out)
}
