# TODO: heckit standard errors are not available because `vcov` is block diagonal with NAs

#' @rdname get_coef
#' @export
get_coef.selection <- function(model, ...) {
    if ("coefficients" %in% names(model)) {
        out <- model$coefficients
    } else if ("estimate" %in% names(model)) {
        out <- model$estimate
    } else {
        stop("Model does not have coefficients or estimates.")
    }
    return(out)
}


#' @rdname set_coef
#' @export
set_coef.selection <- function(model, coefs, ...) {
    # sampleSelection::selection
    if ("coefficients" %in% names(model)) {
        model[["coefficients"]] <- coefs
    } else if ("estimate" %in% names(model)) {
        model[["estimate"]] <- coefs
    } else {
        stop("Model does not have coefficients or estimates.")
    }
    return(model)
}
