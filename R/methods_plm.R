#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
sanitize_model_specific.plm <- function(model, ...) {
    insight::check_if_installed("insight", minimum_version = "0.17.1")
    return(model)
}
