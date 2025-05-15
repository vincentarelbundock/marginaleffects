#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.survreg <- function(model, coefs, ...) {
    #Reverse engineering insight::get_get_parameters.survreg(),
    #which uses summary.survreg()

    nvar0 <- length(model$coefficients)
    nvar <- nrow(model$var)
    if (nvar > nvar0) {
        model[["coefficients"]][] <- coefs[-nvar0]
        model[["scale"]][] <- exp(coefs[nvar0])
    } else {
        model$coefficients[] <- coefs
    }

    model
}

#' @rdname get_predict
#' @export
get_predict.coxph <- function(
    model,
    newdata = insight::get_data(model),
    type = "lp",
    ...
) {
    out <- stats::predict(model, newdata = newdata, type = type, ...)

    out <- data.frame(rowid = seq_len(nrow(newdata)), estimate = out)
    return(out)
}
