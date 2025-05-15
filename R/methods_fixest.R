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
