
#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.afex_aov <- function(model, coefs, ...) {
    mat <- matrix(coefs, ncol = ncol(model$lm$coefficients))
    dimnames(mat) <- dimnames(model$lm$coefficients)
    model$lm$coefficients <- mat
    return(model)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.afex_aov <- function(model, ...) {
    b <- insight::get_parameters(model)
    b <- stats::setNames(b$Estimate, paste(b$Parameter, b$Response, sep = ":"))
    return(b)
}


#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.afex_aov <- function(model, vcov = NULL, ...) {
    if (!is.null(vcov) && !is.logical(vcov)) {
        stop("The `vcov` argument is not supported for models of this class.")
    }
    insight::get_varcov(model)
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.afex_aov <- function(model, newdata = NULL, ...) {
    out <- stats::predict(model, newdata = newdata)
    out <- data.frame(predicted = out)
    if (isTRUE("rowid" %in% colnames(newdata))) {
        out[["rowid"]] <- newdata[["rowid"]]
    } else {
        out[["rowid"]] <- seq_len(nrow(out))
    }
    return(out)
}
