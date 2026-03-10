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
    out <- melt(data.table(out), variable.name = "group", value.name = "estimate")
    data.table::setDF(out)
    out <- add_rowid(out, newdata)
    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.systemfit <- function(model, coefs, ...) {
    out <- model
    out$coefficients <- coefs
    for (i in seq_along(model$eq)) {
        eq <- model$eq[[i]]
        b <- coefs[grepl(paste0("^eq", i, "_"), names(coefs))]
        out$eq[[i]]$coefficients <- b
    }
    return(out)
}
