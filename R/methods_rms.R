#' @rdname get_vcov
#' @export
get_vcov.orm <- function(model,
                         vcov = NULL,
                         ...) {
    if (!is.null(vcov) && !isTRUE(checkmate::check_flag(vcov))) {
        msg <- "The `vcov` argument is not supported for models of this class."
        insight::format_error(msg)
    }
    out <- stats::vcov(model, intercepts = "all")
    return(out)
}
