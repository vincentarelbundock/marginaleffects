get_dydx.multinom <- function(
    model, 
    fitfram, 
    variable, 
    group_name = NULL,
    prediction_type = "probs",
    numDeriv_method = "simple",
    ...) {

    if (is.null(group_name)) {
        stop('You must specify the `group_names` for which to calculate marginal effects. These names are typically the factor levels of the response variable.')
    }

    if (prediction_type != "probs") {
        stop('The only `prediction_type` supported for models of class `multinom` is `"probs"`.')
    }

    out <- list()

    fitfram_tmp <- fitfram
    inner <- function(x) {
        fitfram_tmp[[variable]] <- x
        pred <- predict(model, 
                        newdata = fitfram_tmp, 
                        type = prediction_type)
        pred <- pred[, group_name, drop = FALSE]
        return(pred)
    }
    out <- numDeriv::grad(func = inner, 
                          x = fitfram[[variable]], 
                          method = numDeriv_method)
    return(out)
}

get_dydx_se.multinom <- function(model, 
                                 fitfram, 
                                 variable, 
                                 group_name,
                                 variance, 
                                 numDeriv_method = "simple", 
                                 ...) {

    stop("Variance estimates for models of class `multinom` are not supported yet. Please set `variance=NULL`")

    model_tmp <- model
    inner <- function(x) {
        model_tmp <- reset_coefs(model_tmp, x)
        marginalfx <- get_dydx(model_tmp, 
                               fitfram = fitfram, 
                               variable = variable,
                               group_name = group_name)
        return(marginalfx)
    }
    J <- numDeriv::jacobian(func = inner, 
                            x = coef(model), 
                            method = numDeriv_method)
    out <- se_from_J_V(J, variance)
    return(out)
}
