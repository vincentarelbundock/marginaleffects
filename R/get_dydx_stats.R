get_gradient_glm <- function(model, 
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


get_jacobian_glm <- function(model, 
                             fitfram, 
                             variable, 
                             variance = NULL, 
                             prediction_type = "response",
                             numDeriv_method = "simple", 
                             ...) {
    model_tmp <- model
    inner <- function(x) {
        model_tmp <- reset_coefs(model_tmp, x)
        g <- get_gradient_glm(model = model_tmp,
                              fitfram = fitfram,
                              variable = variable,
                              prediction_type = prediction_type,
                              numDeriv_method = numDeriv_method)
        return(g)
    }
    J <- numDeriv::jacobian(func = inner, 
                            x = stats::coef(model), 
                            method = numDeriv_method)
    return(J)
}


get_dydx.glm <- function(model, 
                         fitfram, 
                         variable, 
                         variance, 
                         prediction_type = "response",
                         numDeriv_method = "simple", 
                         ...) {
    # marginal effects
    g <- get_gradient_glm(model = model,
                          fitfram = fitfram,
                          variable = variable,
                          prediction_type = prediction_type,
                          numDeriv_method = numDeriv_method)
    out <- data.frame(dydx = g)

    # standard errors
    if (!is.null(variance)) {
        J <- get_jacobian_glm(model = model,
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


get_dydx.lm <- get_dydx.glm

get_dydx.loess <- get_dydx.glm
