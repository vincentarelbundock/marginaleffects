#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.mmrm <- function(model, coefs, ...) {
    model$beta_est[names(coefs)] <- coefs
    return(model)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.mmrm <- function(model, ...) {
    return(stats::coef(model, ...))
}


#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.mmrm <- function(model, ...) {
    return(stats::vcov(model, ...))
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.mmrm <- function(model, newdata = insight::get_data(model), type = "response", ...) {
    type <- sanitize_type(model, type, calling_function = "predictions")
    Yhat <- stats::predict(model, newdata = newdata, type = type, conditional = FALSE)
    out <- data.table(estimate = as.vector(Yhat))
    out <- add_rowid(out, newdata)
    return(out)
}


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.mmrm <- function(model, ...) {
    insight::check_if_installed("mmrm", minimum_version = "0.3.14")
    return(model)
}
