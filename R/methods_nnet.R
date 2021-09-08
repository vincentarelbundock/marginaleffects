# TODO: I'm sure this can be simplifed using targeted methods


#' @include reset_coefs.R
#' @rdname reset_coefs
#' @export
reset_coefs.multinom <- function(model, coefs) {
    # internally, coefficients are held in the `wts` vector, with 0s
    # interspersed. When transforming that vector to a matrix, we see that the
    # first row and first column are all zeros. 
    # NOTE: must use `newdata` in predict otherwise returns stored object.
    coefs <- matrix(coefs, nrow = model$n[3L] - 1)
    coefs <- rbind(rep(0, ncol(coefs)), coefs)
    coefs <- cbind(rep(0, nrow(coefs)), coefs)
    model$wts <- as.vector(t(coefs))
    return(model)
}


#' @rdname get_se_delta
#' @export
get_se_delta.multinom <- function(model, 
                                  variable, 
                                  fitfram = insight::find_data(model), 
                                  variance = NULL, 
                                  group_name = NULL,
                                  prediction_type = "probs",
                                  numDeriv_method = "simple", 
                                  ...) {

    model_tmp <- model
    inner <- function(x) {
        model_tmp <- reset_coefs(model_tmp, x)
        g <- get_dydx(model = model_tmp, 
                          fitfram = fitfram, 
                          variable = variable, 
                          group_name = group_name,
                          prediction_type = prediction_type,
                          numDeriv_method = numDeriv_method)
        return(g)
    }
    J <- numDeriv::jacobian(func = inner, 
                            x = as.vector(t(stats::coef(model))),
                            method = numDeriv_method)

    # Var(dydx) = J Var(beta) J'
    # computing the full matrix is memory-expensive, and we only need the diagonal
    # algebra trick: https://stackoverflow.com/a/42569902/342331
    V <- colSums(t(J %*% variance) * t(J))
    se <- sqrt(V)

    return(se)
}
