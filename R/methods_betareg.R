#' @include reset_coefs.R
#' @rdname reset_coefs
#' @export
reset_coefs.betareg <- function(model, coefs) {
    # in betareg, coefficients are a two-element list. We want to substitute the first element!
    # vab: this must reall be named because the precision gets mixed with coefs
    model[["coefficients"]]$mean[names(coefs)] <- coefs
    model
}

#' @rdname get_se_delta
#' @export
get_se_delta.betareg <- function(model, 
                                 fitfram, 
                                 variable, 
                                 variance,
                                 prediction_type = "response",
                                 numDeriv_method = "simple", 
                                 ...) {
    model_tmp <- model
    inner <- function(x) {
        # labelling is the only difference w.r.t. get_se_delta.glm
        x <- stats::setNames(x, names(model_tmp[["coefficients"]][["mean"]]))
        model_tmp <- reset_coefs(model_tmp, x)
        g <- get_mfx(model = model_tmp,
                          fitfram = fitfram,
                          variable = variable,
                          prediction_type = prediction_type,
                          numDeriv_method = numDeriv_method)
        return(g)
    }
    J <- numDeriv::jacobian(func = inner, 
                            x = model_tmp$coefficients$mean,
                            method = numDeriv_method)
    return(J)
}

#' @rdname get_mfx_and_se
#' @export
get_mfx_and_se.betareg <- function(model, 
                             fitfram, 
                             variable, 
                             variance, 
                             prediction_type = "response",
                             numDeriv_method = "simple", 
                             ...) {
    # marginal effects
    g <- get_mfx(model = model,
                      fitfram = fitfram,
                      variable = variable,
                      prediction_type = prediction_type,
                      numDeriv_method = numDeriv_method)
    out <- data.frame(dydx = g)

    # standard errors
    if (!is.null(variance)) {
        J <- get_se_delta(model = model,
                          fitfram = fitfram,
                          variable = variable,
                          prediction_type = prediction_type,
                          numDeriv_method = numDeriv_method)
        # difference with glm due to vcov(model) including precision parameter
        variance <- variance[names(model$coefficients$mean),
                             names(model$coefficients$mean)]
        out$std.error <- se_from_J_V(J, variance)
    }

    # output
    out$rowid <- 1:nrow(fitfram)
    return(out)
}
