delta_se_marginalmeans <- function(model,
                                   variables,
                                   variables_grid,
                                   type,
                                   ...) {
    get_marginalmeans(model = model,
                      variables = variables,
                      variables_grid = variables_grid,
                      type = type,
                      ...)$marginalmean
}

delta_se_marginaleffects <- function(model,
                                     variable,
                                     newdata,
                                     type,
                                     ...) {
    get_dydx(model = model,
             variable = variable,
             newdata = newdata,
             type = type,
             ...)$dydx
}

#' @return vector of standard errors
delta_se <- function(model,
                     vcov,
                     type,
                     FUN,
                     index = NULL,
                     ...) {

    numDeriv_method = "simple"

    # TODO: this is a terrible sanity check
    coefs <- get_coef(model)
    vcov <- vcov[names(coefs), names(coefs), drop = FALSE]

    # input: named vector of coefficients
    # output: gradient
    inner <- function(x) {
        model_tmp <- set_coef(model, stats::setNames(x, names(coefs)))
        g <- FUN(model = model_tmp, type = type, ...)
        return(g)
    }

    J <- numDeriv::jacobian(func = inner,
                            x = coefs,
                            method = numDeriv_method)
    colnames(J) <- names(get_coef(model))

    if (!is.null(index)) {
        J_mean <- stats::aggregate(J, by = index, FUN = mean, na.rm = TRUE)
    } else {
        J_mean <- NULL
    }

    # Unit-level standard errors are much slower to compute (are they, though?)
    # In any case, we need them to match Stata and `margins`
    # Var(dydx) = J Var(beta) J'
    # computing the full matrix is memory-expensive, and we only need the diagonal
    # algebra trick: https://stackoverflow.com/a/42569902/342331
    se <- sqrt(colSums(t(J %*% vcov) * t(J)))


    attr(se, "J") <- J
    attr(se, "J_mean") <- J_mean

    return(se)
}
