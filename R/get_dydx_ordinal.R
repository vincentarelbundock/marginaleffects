# only difference with `get_dydx.clm` is predict()$fit

get_dydx.clm <- function(model, 
                         fitfram, 
                         variable, 
                         prediction_type = "prob",
                         numDeriv_method = "simple",
                         ...) {

    if (prediction_type != "prob") {
        stop('The only `prediction_type` supported for models of class `clm` is `"prob"`.')
    }
    
    fitfram_tmp <- fitfram
    inner <- function(x) {
        fitfram_tmp[[variable]] <- x
        pred <- predict(model, 
                        newdata = fitfram_tmp, 
                        type = prediction_type)$fit
        return(pred)
    }
    out <- numDeriv::grad(func = inner, 
                          x = fitfram[[variable]], 
                          method = numDeriv_method)
    return(out)
}

get_dydx_se.clm <- function(...) {
    stop("The `variance` argument is not supported for models of this class. The `variance` argument must be `NULL`.")
}
