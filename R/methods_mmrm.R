#' @include set_coef.R
#' @rdname set_coef
#' @keywords internal
#' @export
set_coef.mmrm <- function(model, coefs, ...) {
  model$beta_est <- coefs
  return(model)
}


#' @include get_coef.R
#' @rdname get_coef
#' @keywords internal
#' @export
get_coef.mmrm <- function(model, ...) {
  return(coef(model, ...))
}


#' @include get_vcov.R
#' @rdname get_vcov
#' @keywords internal
#' @export
get_vcov.mmrm <- function(model, ...) {
  return(vcov(model, ...))
}


#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.mmrm <- function(model, newdata = model$data, type = "response", ...) {
  type <- match.arg(type)
  res <- data.frame(
    rowid = seq_len(nrow(newdata)),
    estimate = predict(model, newdata = newdata, type = type, conditional = FALSE)
  )
  return(res)
}


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
sanitize_model_specific.mmrm <- function(model, ...) {
  insight::check_if_installed("mmrm", minimum_version = "0.3.14")
  return(model)
}
