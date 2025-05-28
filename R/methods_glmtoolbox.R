#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.glmgee <- function(model, ...) {
    b <- model$coefficients
    b <- stats::setNames(as.vector(b), row.names(b))
    return(b)
}

#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.glmgee <- function(model, coefs, ...) {
    out <- model
    idx <- match(row.names(out$coefficients), names(coefs))
    out$coefficients[, 1] <- coefs[idx]
    return(out)
}

#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.glmgee <- function(model, newdata, ...) {
    Yhat <- stats::predict(model, newdata = newdata, type = "response")
    out <- data.frame(
        rowid = seq_len(nrow(Yhat)),
        estimate = as.vector(Yhat)
    )
    return(out)
}
