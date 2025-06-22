#' @rdname get_predict
#' @export
get_predict.fixest <- function(
    model,
    newdata = insight::get_data(model),
    type = "response",
    ...
) {
    insight::check_if_installed("fixest")

    if (is.null(type)) {
        type <- sanitize_type(
            model = model,
            type = type,
            calling_function = "predictions"
        )
    }

    dots <- list(...)

    # some predict methods raise warnings on unused arguments
    unused <- c(
        "normalize_dydx",
        "step_size",
        "numDeriv_method",
        "conf.int",
        "internal_call"
    )
    dots <- dots[setdiff(names(dots), unused)]

    # fixest is super slow when using do call because of some `deparse()` call
    # issue #531: we don't want to waste time computing intervals or risk having
    # them as leftover columns in contrast computations
    pred <- try(
        stats::predict(
            object = model,
            newdata = newdata,
            type = type
        ),
        silent = TRUE
    )

    if (inherits(pred, "try-error")) {
        return(pred)
    }

    if ("rowid" %in% colnames(newdata)) {
        out <- data.frame(
            rowid = newdata$rowid,
            estimate = as.numeric(pred)
        )
    } else {
        out <- data.frame(
            rowid = seq_len(nrow(newdata)),
            estimate = as.numeric(pred)
        )
    }

    return(out)
}



#' @rdname sanitize_model_specific
sanitize_model_specific.fixest <- function(model, vcov = TRUE, calling_function = "predictions", ...) {
    # issue #1487 is only a problem for standard errors
    if (isFALSE(vcov)) {
        return(model)
    }

    msg <- "For this model type, `marginaleffects` cannot take into account the uncertainty in fixed-effects parameters. Set `vcov=FALSE` to compute estimates without standard errors."

    # issue #1487: fixed-effects always matter for predictions
    if (identical(calling_function, "predictions")) stop_sprintf(msg)

    # issue #1487: fixed-effects matter for slopes and contrasts, except for linear models
    if (!is.null(model$family)) stop_sprintf(msg)

    return(model)
}
