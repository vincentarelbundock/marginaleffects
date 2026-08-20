#' @rdname get_coef
#' @export
get_coef.systemfit <- function(model, ...) {
    out <- stats::coef(model)
    return(out)
}

#' @rdname get_vcov
#' @export
get_vcov.systemfit <- function(model, ...) {
    vcov <- sanitize_vcov(model, vcov)
    out <- stats::vcov(model)
    return(out)
}

#' @rdname get_vcov
#' @export
get_predict.systemfit <- function(model, newdata = NULL, type = NULL, ...) {
    out <- stats::predict(model, newdata = newdata, ...)
    colnames(out) <- sub("\\.pred$", "", colnames(out))
    out <- melt(data.table(out), measure.vars = colnames(out), variable.name = "group", value.name = "estimate")
    data.table::setDF(out)
    out <- add_rowid(out, newdata)
    return(out)
}



#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.systemfit <- function(model, coefs, ...) {
    # `coef()` prefixes each equation's terms with that equation's label, which
    # is `eq1`, `eq2`, ... only when the equations were passed unnamed. Keying
    # off a hard-coded "eq<i>_" prefix wiped the coefficients of every named
    # equation, so match on the stored labels and fall back on position --
    # `coefficients` is laid out equation by equation.
    out <- model
    out$coefficients <- coefs
    pos <- 0L
    for (i in seq_along(model$eq)) {
        b <- model$eq[[i]][["coefficients"]]
        lab <- model$eq[[i]][["eqnLabel"]]
        if (is.null(lab) || !isTRUE(nzchar(lab))) {
            lab <- paste0("eq", i)
        }
        idx <- match(paste0(lab, "_", names(b)), names(coefs))
        if (anyNA(idx)) {
            idx <- pos + seq_along(b)
            if (max(idx) > length(coefs)) {
                stop_sprintf(
                    "Unable to match the coefficients of equation %s in this `systemfit` model.",
                    lab
                )
            }
        }
        out$eq[[i]][["coefficients"]][] <- coefs[idx]
        pos <- pos + length(b)
    }
    return(out)
}
