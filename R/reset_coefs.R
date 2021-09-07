# To compute the variance of marginal effects we need to take the Jacobian with
# respect to the model coefficients. These functions manipulate model objects
# to change the coefficients stored internally, which changes the output of the
# `predict()` function.


##############################################
#  copied from `margins` under MIT license  ##
#  copyright: Thomas J. Leeper 2016-2018    ##
##############################################

#' utility function to reset the coefficients in a model object, and return the modified model
#' @export
reset_coefs <- function(model, coefs) {
    UseMethod("reset_coefs")
}

reset_coefs.default <- function(model, coefs) {
    # in basic model classes coefficients are named vector
    model[["coefficients"]][names(coefs)] <- coefs
    model
}

reset_coefs.lm <- function(model, coefs) {
    # in lm coefficients are named vector
    model[["coefficients"]][names(coefs)] <- coefs
    model
}

reset_coefs.glm <- function(model, coefs) {
    # in glm coefficients are named vector
    model[["coefficients"]][names(coefs)] <- coefs
    ## But, there's an edge case!! When `predict(model, se.fit = TRUE)` is called without `newdata`, `predict.lm()` isn't called.
    ## Instead `model$linear.predictors` is returned directly if `type = "link"` and
    ## `model$fitted.values` is returned directly if `type = "response"`.
    ## `marginal_effects()` for "glm" is always called with `newdata`, so we won't hit this.
    model
}

#' @export
reset_coefs.betareg <- function(model, coefs) {
    # in betareg, coefficients are a two-element list. We want to substitute the first element!
    model[["coefficients"]]$mean[names(coefs)] <- coefs
    model
}

reset_coefs.merMod <- function(model, coefs) {
    # in 'merMod', predictions work the slot called "beta", which is unnamed
    # `fixef(model)` returns the same thing named
    requireNamespace("methods")
    beta <- methods::slot(model, "beta")
    beta[match(names(coefs), names(lme4::fixef(model)))] <- as.numeric(coefs)
    methods::slot(model, "beta") <- beta
    model
}

reset_coefs.lmerMod <- reset_coefs.merMod


###########################################
#  Below written by Vincent Arel-Bundock  #
###########################################

#' @export
reset_coefs.multinom <- function(model, coefs) {
    # internally, coefficients are held in the `wts` vector, with 0s
    # interspersed. When transforming that vector to a matrix, we see that the
    # first row and first column are all zeros. 
    # NOTE: must use `newdata` in predict otherwise returns stored object.
    coefs <- matrix(coefs, nrow = model$n[3L] - 1)
    coefs <- rbind(rep(0, ncol(coefs)), coefs)
    coefs <- cbind(rep(0, nrow(coefs)), coefs)
    model$wts <- as.vector(t(coefs))
    return(model)
}
