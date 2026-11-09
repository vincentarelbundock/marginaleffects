# glmgee -----------------------------------------------------------------------

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
get_predict.glmgee <- function(model, newdata, type = "response", ...) {
    type <- sanitize_type(model, type, calling_function = "predictions")
    Yhat <- stats::predict(model, newdata = newdata, type = type)
    out <- data.table(estimate = as.vector(Yhat))
    out <- add_rowid(out, newdata)
    return(out)
}


# gnm -------------------------------------------------------------------------

#' @rdname get_coef
#' @export
get_coef.gnm <- get_coef.glmgee

#' @rdname set_coef
#' @export
set_coef.gnm <- set_coef.glmgee

#' @rdname get_predict
#' @export
get_predict.gnm <- get_predict.glmgee
