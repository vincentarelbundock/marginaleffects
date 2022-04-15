standard_errors_delta_marginalmeans <- function(model,
                                                variables,
                                                newdata,
                                                type,
                                                ...) {
    get_marginalmeans(model = model,
                      variables = variables,
                      newdata = newdata,
                      type = type,
                      ...)$marginalmean
}


standard_errors_delta_contrasts <- function(model,
                                            variables,
                                            newdata,
                                            type,
                                            contrast_factor,
                                            contrast_numeric,
                                            ...) {
    get_contrasts(model,
                  newdata = newdata,
                  variables = variables,
                  type = type,
                  contrast_factor = contrast_factor,
                  contrast_numeric = contrast_numeric,
                  ...)$comparison
}



#' Compute standard errors using the delta method
#'
#' @inheritParams marginaleffects
#' @param FUN a function which accepts a `model` and other inputs and returns a
#'   vector of estimates (marginal effects, marginal means, etc.)
#' @param index data.frame over which we aggregate J_mean (matches tidy() output)
#' @return vector of standard errors
#' @noRd
standard_errors_delta <- function(model,
                                  vcov,
                                  type,
                                  newdata,
                                  FUN,
                                  index = NULL,
                                  ...) {

    # delta method does not work for these models
    bad <- c("brmsfit", "stanreg")
    if (any(bad %in% class(model))) {
        return(NULL)
    }

    coefs <- get_coef(model)

    # TODO: this is a terrible sanity check
    # some vcov methods return an unnamed matrix
    if (!is.null(dimnames(vcov))) {
        vcov <- vcov[names(coefs), names(coefs), drop = FALSE]
    }

    # input: named vector of coefficients
    # output: gradient
    inner <- function(x) {
        model_tmp <- set_coef(model, stats::setNames(x, names(coefs)))
        g <- FUN(model = model_tmp, newdata = newdata, type = type, ...)
        return(g)
    }

    J <- get_jacobian(func = inner, x = coefs)

    colnames(J) <- names(get_coef(model))

    # Var(dydx) = J Var(beta) J'
    # computing the full matrix is memory-expensive, and we only need the diagonal
    # algebra trick: https://stackoverflow.com/a/42569902/342331
    se <- sqrt(colSums(t(J %*% vcov) * t(J)))


    attr(se, "J") <- J

    return(se)
}
