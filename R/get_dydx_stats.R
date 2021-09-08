################
#  stats::glm  #
################

#' @include reset_coefs.R
#' @rdname reset_coefs
#' @export
reset_coefs.glm <- function(model, coefs) {
    # in glm coefficients are named vector
    model[["coefficients"]][names(coefs)] <- coefs
    ## But, there's an edge case!! When `predict(model, se.fit = TRUE)` is called without `newdata`, `predict.lm()` isn't called.
    ## Instead `model$linear.predictors` is returned directly if `type = "link"` and
    ## `model$fitted.values` is returned directly if `type = "response"`.
    ## `marginal_effects()` for "glm" is always called with `newdata`, so we won't hit this.
    model
}


###############
#  stats::lm  #
###############

#' @rdname reset_coefs
#' @export
reset_coefs.lm <- function(model, coefs) {
    # in lm coefficients are named vector
    model[["coefficients"]][names(coefs)] <- coefs
    model
}


##################
#  stats::loess  #
##################
