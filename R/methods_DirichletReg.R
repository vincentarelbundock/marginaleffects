#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.DirichletRegModel <- function(model, ...) {
    coefs <- model$coefficients
    return(coefs)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.DirichletRegModel <- function(model, coefs, ...) {
    model$coefficients <- coefs
    return(model)
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.DirichletRegModel <- function(model, newdata, ...) {
    pred <- stats::predict(model, newdata = newdata, ...)
    if (is.matrix(pred)) {
        if (is.null(colnames(pred))) {
            if ("varnames" %in% names(model)) {
                colnames(pred) <- model$varnames
            } else {
                colnames(pred) <- seq_len(ncol(pred))
            }
        }
        out <- data.frame(
            group = rep(colnames(pred), each = nrow(pred)),
            estimate = as.vector(pred)
        )
        out$group <- group_to_factor(out$group, model)
    } else if (is.atomic(pred)) {
        out <- data.frame(estimate = as.vector(pred))
    } else {
        stop("Unexpected prediction output type from DirichletReg::predict().")
    }
    out <- add_rowid(out, newdata)
    return(out)
}
