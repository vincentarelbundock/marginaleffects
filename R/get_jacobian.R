#' @title Get Jacobian
#' @rdname get_jacobian
#' @export
get_jacobian <- function (model, ...) {
    UseMethod("get_jacobian", model)
}


#' @rdname get_jacobian
#' @export
get_jacobian.default <- function(model, 
                                 fitfram, 
                                 variable, 
                                 prediction_type = "response",
                                 numDeriv_method = "simple", 
                                 ...) {
    model_tmp <- model
    coefs <- get_coef(model)
    inner <- function(x) {
        model_tmp <- reset_coefs(model_tmp, stats::setNames(x, names(coefs)))
        g <- get_gradient(model = model_tmp,
                          fitfram = fitfram,
                          variable = variable,
                          prediction_type = prediction_type,
                          numDeriv_method = numDeriv_method)
        return(g)
    }
    J <- numDeriv::jacobian(func = inner, 
                            x = coefs,
                            method = numDeriv_method)
    return(J)
}
