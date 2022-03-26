#' @include sanity_model.R
#' @rdname sanity_model_specific
#' @keywords internal
sanity_model_specific.lmRob <- function(model, ...) {
    termlabs <- try(attr(stats::terms(model), "term.labels"), silent = TRUE)
    termlabs <- try(any(grepl("^factor\\(|^as.factor\\(|^as.logical\\(", termlabs)), silent = TRUE)
    if (isTRUE(termlabs)) {
        warning("When using `marginaleffects` functions, it is safer to convert variables to factors or logicals in the dataset before fitting the model, rather than by wrapping terms in `factor()` or `as.logical() in the model formula.",
                call. = FALSE)
    }
}
