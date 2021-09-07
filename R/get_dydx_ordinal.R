# only difference with `get_dydx.glm` is predict()$fit

get_dydx.clm <- function(model, 
                         fitfram, 
                         variable, 
                         variance = NULL,
                         prediction_type = "prob",
                         numDeriv_method = "simple",
                         ...) {

    if (prediction_type != "prob") {
        stop('The only `prediction_type` supported for models of class `clm` is `"prob"`.')
    }
    
    fitfram_tmp <- fitfram
    inner <- function(x) {
        fitfram_tmp[[variable]] <- x
        pred <- stats::predict(model, 
                               newdata = fitfram_tmp, 
                               type = prediction_type)$fit
        return(pred)
    }
    g <- numDeriv::grad(func = inner, 
                        x = fitfram[[variable]], 
                        method = numDeriv_method)
    out <- data.frame(rowid = 1:nrow(fitfram), term = variable, dydx = g)

    if (!is.null(variance)) {
        stop('The `variance` argument is not yet supported for models of class `clm`.')
    }

    # output
    return(out)
}
