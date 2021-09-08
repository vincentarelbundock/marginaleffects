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
                                 variance,
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

    # Var(dydx) = J Var(beta) J'
    # computing the full matrix is memory-expensive, and we only need the diagonal
    # algebra trick: https://stackoverflow.com/a/42569902/342331
    V <- colSums(t(J %*% variance) * t(J))
    se <- sqrt(V)

    return(se)
}
