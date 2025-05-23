#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.bart <- function(model, newdata = NULL, ...) {
    args <- c(
        list(
            object = model,
            newdata = newdata
        ),
        list(...)
    )
    p <- do.call(stats::predict, args)
    p_med <- collapse::fmedian(p)
    if ("rowid" %in% colnames(newdata) && nrow(newdata) == length(p_med)) {
        out <- data.frame(
            rowid = newdata$rowid,
            group = "main_marginaleffect",
            estimate = p_med
        )
    } else {
        out <- data.frame(
            rowid = seq_along(length(p_med)),
            group = "main_marginaleffect",
            estimate = p_med
        )
    }
    attr(out, "posterior_draws") <- t(p)
    return(out)
}


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.bart <- function(model, ...) {
    insight::check_if_installed("collapse", minimum_version = "1.9.0")
    if (
        !isTRUE(
            as.character(insight::get_call(model))[1] %in% c("bart2", "dbarts::bart2")
        )
    ) {
        msg <- "`marginaleffects` only supports models estimated using the formula interface in `bart2()` function, not the matrix input in `bart()`."
        insight::format_error(msg)
    }
    return(model)
}


#' @rdname get_vcov
#' @export
get_vcov.bart <- function(model, vcov = NULL, ...) {
    if (!is.null(vcov) && !is.logical(vcov)) {
        insight::format_warning(
            "The `vcov` argument is not supported for models of this class."
        )
    }
    vcov <- sanitize_vcov(model, vcov)
    return(NULL)
}
