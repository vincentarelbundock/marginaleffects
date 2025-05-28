#' @rdname get_vcov
#' @export
get_vcov.orm <- function(model, vcov = NULL, ...) {
    if (!is.null(vcov) && !isTRUE(checkmate::check_flag(vcov))) {
        msg <- "The `vcov` argument is not supported for models of this class."
        insight::format_error(msg)
    }
    vcov <- sanitize_vcov(model, vcov)
    out <- stats::vcov(model, intercepts = "all")
    return(out)
}


#' @rdname get_predict
#' @export
get_predict.rms <- function(
    model,
    newdata = insight::get_data(model),
    type = NULL,
    ...
) {
    if (is.null(type)) {
        type <- sanitize_type(model, type, calling_function = "predictions")
    }
    if (inherits(newdata, "tbl_df")) {
        warning("Converting `newdata` from tibble to data.frame.", call. = FALSE)
        newdata <- as.data.frame(newdata)
    }

    # {rms} predict methods break on additional arguments
    get_predict.default(model, newdata = newdata, type = type)
}

#' @rdname get_predict
#' @export
get_predict.orm <- get_predict.rms


#' @rdname get_predict
#' @export
get_predict.lrm <- get_predict.rms


#' @rdname get_predict
#' @export
get_predict.ols <- get_predict.rms
