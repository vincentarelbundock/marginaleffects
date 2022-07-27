#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.glm <- function(model, coefs, ...) {
    # in glm coefficients are named vector
    model[["coefficients"]][names(coefs)] <- coefs
    ## But, there's an edge case!! When `predict(model, se.fit = TRUE)` is called without `newdata`, `predict.lm()` isn't called.
    ## Instead `model$linear.predictors` is returned directly if `type = "link"` and
    ## `model$fitted.values` is returned directly if `type = "response"`.
    ## `marginal_effects()` for "glm" is always called with `newdata`, so we won't hit this.
    model
}

#' @rdname set_coef
#' @export
set_coef.lm <- function(model, coefs, ...) {
    # in lm coefficients are named vector
    model[["coefficients"]][names(coefs)] <- coefs
    model
}
