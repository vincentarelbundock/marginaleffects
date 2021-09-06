#' @include get_dydx_glm.R
get_dydx.loess <- get_dydx.glm

get_dydx_se.loess <- function(model, 
                              fitfram, 
                              variable, 
                              variance, 
                              numDeriv_method = "simple", 
                              ...) {
    if (!is.null(variance)) {
        stop("The `variance` argument is not supported for models of type `loess`. It must be `NULL`.")
    }
    get_dydx.glm(model = model,
                 fitfram = fitfram,
                 variable = variable,
                 variance = variance,
                 numDeriv_method = numDeriv_method,
                 ...)
}


