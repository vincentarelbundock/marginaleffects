#' @rdname get_predict
#' @export
get_predict.mhurdle <- function(
    model,
    newdata = get_modeldata(model),
    type = "response",
    ...) {
    out <- stats::predict(model, what = type, newdata = newdata)
    out <- data.table(estimate = out)
    out <- add_rowid(out, newdata)
    return(out)
}


#' @rdname get_vcov
#' @export
get_vcov.mhurdle <- function(model, vcov = NULL, ...) {
    if (!is.null(vcov) && !is.logical(vcov)) {
        stop_sprintf(
            "The `vcov` for this class of models must be TRUE or FALSE."
        )
    }
    vcov <- sanitize_vcov(model, vcov)
    out <- try(stats::vcov(model), silent = TRUE)
    if (inherits(out, "try-error")) {
        out <- tryCatch(model[["vcov"]], error = function(e) NULL)
    }
    return(out)
}


#' @rdname set_coef
#' @export
set_coef.mhurdle <- function(model, coefs, ...) {
    # `coef.mhurdle()` relabels and reorders the stored parameter vector: the
    # scale parameter is called `sd` in `model$coefficients` but `sd.sd` in
    # `coef()`, `vcov()`, and therefore `get_coef()`. Name-matching against the
    # stored vector would append a stray `sd.sd` element and leave the real
    # scale parameter untouched, zeroing out its Jacobian column. Probe the
    # relabeling by running `coef()` on a copy whose values are the positions
    # they occupy in `model$coefficients`.
    probe <- model
    probe[["coefficients"]] <- stats::setNames(
        seq_along(model[["coefficients"]]),
        names(model[["coefficients"]])
    )
    pos <- stats::coef(probe)
    idx <- match(names(coefs), names(pos))
    if (anyNA(idx)) {
        return(set_coef.default(model, coefs, ...))
    }
    model[["coefficients"]][pos[idx]] <- coefs
    model
}
