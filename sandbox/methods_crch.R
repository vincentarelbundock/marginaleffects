#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.crch <- function(model, coefs) {
    # in crch::crch, coefficients are held in:
    # model$coefficients$location
    # model$coefficients$scale
    out <- model

    idx <- grepl("^\\(scale\\)_", names(get_coef(model)))
    coefs_location <- coefs[!idx]
    coefs_scale <- coefs[idx]
    names(coefs_scale) <- gsub("^\\(scale\\)_", "", names(coefs_scale))

    out$coefficients$scale <- coefs_scale
    out$coefficients$location <- coefs_location

    return(out)
}
