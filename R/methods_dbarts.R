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
    p <- safe_do_call(stats::predict, args)
    p_med <- collapse::fmedian(p)
    out <- data.table(
        group = "main_marginaleffect",
        estimate = p_med)
    out <- add_rowid(out, newdata)
    attr(out, "posterior_draws") <- t(p)
    return(out)
}


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.bart <- function(model, calling_function, ...) {
    insight::check_if_installed("collapse", minimum_version = "1.9.0")
    if (
        !isTRUE(
            as.character(insight::get_call(model))[1] %in% c("bart2", "dbarts::bart2")
        )
    ) {
        msg <- "`marginaleffects` only supports models estimated using the formula interface in `bart2()` function, not the matrix input in `bart()`."
        stop_sprintf(msg)
    }
    if (calling_function == "hypotheses") {
        msg <- "`marginaleffects` does not support hypothesis tests for models of class `bart`."
        stop_sprintf(msg)
    }
    return(model)
}


#' @rdname get_vcov
#' @export
get_vcov.bart <- function(model, vcov = NULL, ...) {
    if (!is.null(vcov) && !is.logical(vcov)) {
        warn_sprintf(
            "The `vcov` argument is not supported for models of this class."
        )
    }
    vcov <- sanitize_vcov(model, vcov)
    return(NULL)
}
