get_dydx.glm <- function(model, 
                         fitfram, 
                         variable, 
                         prediction_type = "response",
                         numDeriv_method = "simple",
                         ...) {
    fitfram_tmp <- fitfram
    inner <- function(x) {
        fitfram_tmp[[variable]] <- x
        pred <- predict(model, 
                        newdata = fitfram_tmp, 
                        type = prediction_type)
        # predict(survey::svyglm) produces a vector with weird attributes
        pred <- as.numeric(pred)
        return(pred)
    }
    out <- numDeriv::grad(func = inner, 
                          x = fitfram[[variable]], 
                          method = numDeriv_method)
    return(out)
}

get_dydx_se.glm <- function(model, 
                            fitfram, 
                            variable, 
                            variance, 
                            numDeriv_method = "simple", 
                            ...) {
    model_tmp <- model
    inner <- function(x) {
        model_tmp <- reset_coefs(model_tmp, x)
        marginalfx <- get_dydx(model_tmp, 
                               fitfram = fitfram, 
                               variable = variable)
        return(marginalfx)
    }
    J <- numDeriv::jacobian(func = inner, 
                            x = coef(model), 
                            method = numDeriv_method)
    out <- se_from_J_V(J, variance)
    return(out)
}
