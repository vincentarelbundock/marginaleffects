#' @include sanity_model.R
#' @rdname sanity_model_specific
#' @keywords internal
sanity_model_specific.plm <- function(model, ...) {
    if ("within" %in% model$args$model) {
        stop('The `plm::predict` function does not appear to support the `newdata` argument when `plm(model="within")`. Therefore, `marginaleffects` cannot support "within" models, even if it supports many other models produced by the `plm` package. You may want to try the `fixest` package instead.',
             call. = FALSE)
    }
}


#' @rdname sanity_model_specific
sanity_model_specific.plm <- function(model, ...) {
    insight::check_if_installed("insight", minimum_version = "0.17.1")
}
