#' @include get_dydx_stats.R
#' @rdname get_gradient
#' @export
get_gradient.clm <- get_gradient.glm


#' @rdname reset_coefs
#' @export
reset_coefs.clm <- reset_coefs.default


#' @rdname get_jacobian
#' @export
get_jacobian.clm <- get_jacobian.glm


#' @rdname get_dydx
#' @export
# The only difference with `get_dydx.glm` is predict()$fit
get_dydx.clm <- function(model, 
                         fitfram, 
                         variable, 
                         variance = NULL,
                         prediction_type = "prob",
                         numDeriv_method = "simple",
                         ...) {

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


    # output
    return(out)
}
