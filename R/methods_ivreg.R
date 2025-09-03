#' @keywords internal
#' @export
get_autodiff_args.ivreg <- function(model, mfx) {
    # no inheritance! Important to avoid breaking other models
    if (!class(model)[1] == "ivreg") {
        return(NULL)
    }

    if (!is.null(model$offset)) {
        autodiff_warning("models with offsets")
        return(NULL)
    }

    # If all checks pass, return supported arguments
    out <- list(model_type = "linear")
    return(out)
}
