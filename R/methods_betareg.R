#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.betareg <- function(model, coefs) {
    # in betareg, coefficients are a two-element list. We want to substitute the first element!
    # vab: this must absolutely be a named vector, otherwise the precision gets mixed with coefs
    model[["coefficients"]]$mean[names(coefs)] <- coefs
    model
}

#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.betareg <- function(model, ...) {
    model$coefficients$mean
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.betareg <- function(model, newdata, ...) {
    out <- stats::predict(model, newdata = newdata)
    out <- data.frame(rowid = seq_len(nrow(newdata)),
                      predicted = out)
    return(out)
}


#' @rdname sanity_model_specific
sanity_model_specific.betareg <- function(model, ...) {
    insight::check_if_installed("insight", minimum_version = "0.17.1")
}

