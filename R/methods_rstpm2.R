#' @rdname set_coef
#' @export
set_coef.stpm2 <- function(model, coefs, ...) {
    insight::check_if_installed("rstpm2")
    model@fullcoef <- coefs
    model
}

#' @rdname set_coef
#' @export
set_coef.pstpm2 <- set_coef.stpm2

#' @rdname set_coef
#' @export
set_coef.gsm <- set_coef.stpm2


#' @rdname set_coef
#' @export
set_coef.aft <- set_coef.stpm2


#' @rdname get_vcov
#' @export
get_vcov.stpm2 <- function(model, ...) {
    insight::check_if_installed("rstpm2")
    vcov <- sanitize_vcov(model, vcov)
    rstpm2::vcov(model)
}

#' @rdname get_vcov
#' @export
get_vcov.pstpm2 <- get_vcov.stpm2

#' @rdname get_vcov
#' @export
get_vcov.gsm <- get_vcov.stpm2

#' @rdname get_vcov
#' @export
get_vcov.aft <- get_vcov.stpm2


#' @rdname get_predict
#' @export
get_predict.stpm2 <- function(model, newdata = NULL, ...) {
    insight::check_if_installed("rstpm2")
    pred <- rstpm2::predict(model, newdata = newdata, ...)
    sanity_predict_vector(pred = pred, model = model, newdata = newdata)
    sanity_predict_numeric(pred = pred, model = model, newdata = newdata)
    out <- data.frame(
        rowid = seq_len(nrow(newdata)),
        estimate = pred)
    return(out)
}

#' @rdname get_predict
#' @export
get_predict.pstpm2 <- get_predict.stpm2

#' @rdname get_predict
#' @export
get_predict.gsm <- get_predict.stpm2

#' @rdname get_predict
#' @export
get_predict.aft <- get_predict.stpm2
