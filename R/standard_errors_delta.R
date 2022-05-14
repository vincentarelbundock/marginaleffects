standard_errors_delta_marginalmeans <- function(model,
                                                variables,
                                                newdata,
                                                type,
                                                eps = 1e-4, # avoid pushing through ...
                                                interaction = FALSE,
                                                ...) {
    get_marginalmeans(model = model,
                      variables = variables,
                      newdata = newdata,
                      type = type,
                      interaction = interaction,
                      ...)$marginalmean
}


standard_errors_delta_contrasts <- function(model,
                                            variables,
                                            newdata,
                                            type,
                                            transform_pre,
                                            contrast_factor,
                                            contrast_numeric,
                                            eps,
                                            ...) {
    get_contrasts(model,
                  newdata = newdata,
                  variables = variables,
                  type = type,
                  transform_pre = transform_pre,
                  contrast_factor = contrast_factor,
                  contrast_numeric = contrast_numeric,
                  eps = eps,
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
                                  eps = 1e-4,
                                  ...) {

    # delta method does not work for these models
    bad <- c("brmsfit", "stanreg")
    if (any(bad %in% class(model))) {
        return(NULL)
    }

    coefs <- get_coef(model)

    # TODO: this is a terrible sanity check
    # some vcov methods return an unnamed matrix
    if (!is.null(dimnames(vcov)) && all(names(coefs) %in% colnames(vcov))) {
        vcov <- vcov[names(coefs), names(coefs), drop = FALSE]
    }

    # input: named vector of coefficients
    # output: gradient
    inner <- function(x) {
        model_tmp <- set_coef(model, stats::setNames(x, names(coefs)))
        g <- FUN(model = model_tmp, newdata = newdata, type = type, eps = eps, ...)
        return(g)
    }

    J <- get_jacobian(func = inner, x = coefs, eps = eps)

    colnames(J) <- names(get_coef(model))

    # Var(dydx) = J Var(beta) J'
    # computing the full matrix is memory-expensive, and we only need the diagonal
    # algebra trick: https://stackoverflow.com/a/42569902/342331
    JV <- align_J_V(J, vcov)
    se <- sqrt(colSums(t(JV$J %*% JV$V) * t(JV$J)))

    attr(se, "J") <- JV$J

    return(se)
}
