#' @include set_coef.R
#' @rdname set_coef
#' @keywords internal
#' @export
set_coef.mmrm <- function(model, coefs, ...) {
  model$beta_est  <- coefs
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
  response_var <- all.vars(fit$formula_parts$formula[[2]])
  newdata[[response_var]] <- NA_real_
  res <- data.frame(
    rowid = seq_len(nrow(newdata)),
    estimate = predict(model, newdata = newdata, ...)
  )
  return(res)
}
