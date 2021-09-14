#' @rdname get_vcov
#' @export
get_vcov.selection <- function(model, ...) {
    # sampleSelection::selection
    if (as.list(model$call)[[1]] == "selection") {
        out <- model$estimate
    # sampleSelection::heckit
    } else if (as.list(model$call)[[1]] == "heckit") {
        out <- model$coefficients
        out[is.na(out)] <- 0
    }
    return(out)
}


#' @rdname get_coef
#' @export
get_coef.selection <- function(model, ...) {
    # sampleSelection::selection
    if (as.list(model$call)[[1]] == "selection") {
        out <- model$estimate
    # sampleSelection::heckit
    } else if (as.list(model$call)[[1]] == "heckit") {
        out <- model$coefficients
    }
    return(out)
}


#' @rdname set_coef
#' @export
set_coef.selection <- function(model, coefs) {
    # sampleSelection::selection
    if (as.list(model$call)[[1]] == "selection") {
        model[["estimate"]][names(coefs)] <- coefs
    # sampleSelection::heckit
    } else if (as.list(model$call)[[1]] == "heckit") {
        model[["coefficients"]][names(coefs)] <- coefs
    }
    return(model)
}
