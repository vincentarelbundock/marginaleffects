################
#  stats::glm  #
################

#' @rdname get_gradient
#' @export
get_gradient.glm <- function(model, 
                             fitfram, 
                             variable, 
                             prediction_type = "response",
                             numDeriv_method = "simple") {
    fitfram_tmp <- fitfram
    inner <- function(x) {
        fitfram_tmp[[variable]] <- x
        pred <- stats::predict(model, 
                               newdata = fitfram_tmp, 
                               type = prediction_type)
        # svyglm re-uses this method and predict.svyglm has weird attributes
        pred <- as.numeric(pred)
        return(pred)
    }
    g <- numDeriv::grad(func = inner, 
                        x = fitfram[[variable]], 
                        method = numDeriv_method)
    return(g)
}


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


#' @rdname get_jacobian
#' @export
get_jacobian.glm <- function(model, 
                             fitfram, 
                             variable, 
                             prediction_type = "response",
                             numDeriv_method = "simple", 
                             ...) {
    model_tmp <- model
    inner <- function(x) {
        model_tmp <- reset_coefs(
            model_tmp, stats::setNames(x, names(stats::coef(model))))
        g <- get_gradient(model = model_tmp,
                          fitfram = fitfram,
                          variable = variable,
                          prediction_type = prediction_type,
                          numDeriv_method = numDeriv_method)
        return(g)
    }
    J <- numDeriv::jacobian(func = inner, 
                            x = stats::coef(model_tmp), 
                            method = numDeriv_method)
    return(J)
}


#' @rdname get_dydx
#' @export
get_dydx.glm <- function(model, 
                         fitfram, 
                         variable, 
                         variance, 
                         prediction_type = "response",
                         numDeriv_method = "simple", 
                         ...) {
    # marginal effects
    g <- get_gradient(model = model,
                      fitfram = fitfram,
                      variable = variable,
                      prediction_type = prediction_type,
                      numDeriv_method = numDeriv_method)
    out <- data.frame(dydx = g)

    # standard errors
    if (!is.null(variance)) {
        J <- get_jacobian(model = model,
                          fitfram = fitfram,
                          variable = variable,
                          prediction_type = prediction_type,
                          numDeriv_method = numDeriv_method)
        out$std.error <- se_from_J_V(J, variance)
    }

    # output
    out$rowid <- 1:nrow(fitfram)
    return(out)
}


###############
#  stats::lm  #
###############

#' @rdname get_gradient
#' @export
get_gradient.lm <- get_gradient.glm

#' @rdname reset_coefs
#' @export
reset_coefs.lm <- function(model, coefs) {
    # in lm coefficients are named vector
    model[["coefficients"]][names(coefs)] <- coefs
    model
}

#' @rdname get_jacobian
#' @export
get_jacobian.lm <- get_jacobian.glm


#' @rdname get_jacobian
#' @export
get_dydx.lm <- get_dydx.glm


##################
#  stats::loess  #
##################

#' @rdname get_gradient
#' @export
get_gradient.loess <- get_gradient.glm


#' @rdname get_dydx
#' @export
get_dydx.loess <- get_dydx.glm
