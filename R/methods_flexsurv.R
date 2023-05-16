#' @rdname get_coef
#' @export
get_coef.flexsurvreg <- function(model, ...) {
  return(stats::coef(model))
}

#' @rdname set_coef
#' @export
set_coef.flexsurvreg <- function(model, coefs, ...) {
  out <- model
  out$res[, 1] <- coefs
  out$coefficients <- coefs
  return(out)
}

#' @rdname get_vcov
#' @export
get_vcov.flexsurvreg <- function(model, ...) {
  return(stats::vcov(model))
}

#' @rdname get_predict
#' @export
get_predict.flexsurvreg <- function(model, newdata, type, times, ...) {
  checkmate::assert_scalar(checkmate::assert_double(times))
  preds <- stats::predict(
    object = model,
    newdata = newdata,
    type = type,
    times = times,
    ...
  )
  out <- data.frame(
    rowid = seq_len(nrow(preds)),
    estimate = as.vector(preds[, 2])
  )
  return(out)
}
