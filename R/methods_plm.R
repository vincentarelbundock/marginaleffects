#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
sanitize_model_specific.plm <- function(model, ...) {
    if ("within" %in% model$args$model) {
        stop(
            'The `plm::predict` function does not appear to support the `newdata` argument when `plm(model="within")`. Therefore, `marginaleffects` cannot support "within" models, even if it supports many other models produced by the `plm` package. You may want to try the `fixest` package instead.',
            call. = FALSE
        )
    }
    return(model)
}


#' @rdname sanitize_model_specific
sanitize_model_specific.plm <- function(model, ...) {
    insight::check_if_installed("insight", minimum_version = "0.17.1")
    return(model)
}
