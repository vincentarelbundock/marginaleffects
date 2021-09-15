#' Compute a vector of standard errors using the delta method (internal function)
#'
#' @rdname get_se_delta
#' @return A vector of standard errors of the same lenght as the coefficient
#' @inheritParams marginaleffects
#' @inheritParams get_dydx_and_se
#' @keywords internal
#' @export
get_se_delta <- function (model, ...) {
    UseMethod("get_se_delta", model)
}


#' @rdname get_se_delta
#' @export
get_se_delta.default <- function(model, 
                                 variable,
                                 fitfram = insight::get_data(model), 
                                 vcov = stats::vcov(model),
                                 group_name = NULL,
                                 predict_type = "response",
                                 numDeriv_method = "simple", 
                                 ...) {

    model_tmp <- model
    coefs <- get_coef(model)
    vcov <- vcov[names(coefs), names(coefs)]

    if (is.factor(fitfram[[variable]]) || is.logical(fitfram[[variable]])) {
        dydx_fun <- get_dydx_categorical
    } else {
        print("good")
        dydx_fun <- get_dydx_continuous
    }

    # Jacobian
    inner <- function(x) {
        model_tmp <- set_coef(model_tmp, stats::setNames(x, names(coefs)))
        g <- dydx_fun(model = model_tmp,
                      fitfram = fitfram,
                      variable = variable,
                      group_name = group_name,
                      predict_type = predict_type,
                      numDeriv_method = numDeriv_method)
        return(g$dydx)
    }
    J <- numDeriv::jacobian(func = inner, 
                            x = coefs,
                            method = numDeriv_method)

    # Standard error
    # Var(dydx) = J Var(beta) J'
    # computing the full matrix is memory-expensive, and we only need the diagonal
    # algebra trick: https://stackoverflow.com/a/42569902/342331
    V <- colSums(t(J %*% vcov) * t(J))
    se <- sqrt(V)

    return(se)
}
