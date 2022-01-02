get_dydx_se <- function(model,
                        mfx,
                        vcov,
                        variable,
                        newdata,
                        type,
                        numDeriv_method,
                        ...) {

    coefs <- get_coef(model)

    if (is.null(vcov) ||
        !all(names(coefs) %in% colnames(vcov)) ||
        !all(colnames(vcov) %in% names(coefs))) {
        return(mfx)
    }

    # no delta method for bayesian models because we cannot manipulate the
    # coefficients of the model. use posterior draws instead.
    if (inherits(model, "brmsfit") || inherits(model, "stanreg")) {
        return(mfx)
    }

    vcov <- vcov[names(coefs), names(coefs), drop = FALSE]

    out <- mfx

    if (variable %in% find_categorical(newdata) || isTRUE(attr(newdata[[variable]], "factor"))) {
        dydx_fun <- get_contrasts
    } else {
        dydx_fun <- get_dydx_continuous
    }

    inner <- function(x) {
        model_tmp <- set_coef(model, stats::setNames(x, names(coefs)))
        g <- dydx_fun(model = model_tmp,
                      newdata = newdata,
                      variable = variable,
                      type = type,
                      numDeriv_method = numDeriv_method)
        if (all(c("term", "contrast", "estimate") %in% colnames(g))) {
            colnames(g)[colnames(g) == "estimate"] <- "dydx"
        }
        return(g$dydx)
    }

    J <- numDeriv::jacobian(func = inner,
                            x = coefs,
                            method = numDeriv_method)
    colnames(J) <- names(get_coef(model))


    idx <- intersect(c("term", "group", "contrast"), colnames(out))
    J_mean <- stats::aggregate(J, by = out[, idx], FUN = mean, na.rm = TRUE)

    # Unit-level standard errors are much slower to compute (are they, though?)
    # In any case, we need them to match Stata and `margins`
    # Var(dydx) = J Var(beta) J'
    # computing the full matrix is memory-expensive, and we only need the diagonal
    # algebra trick: https://stackoverflow.com/a/42569902/342331
    V <- colSums(t(J %*% vcov) * t(J))
    out$std.error <- sqrt(V)

    # Keep row.names for J and J_mean matrices
    attr(out, "J") <- J
    attr(out, "J_mean") <- J_mean

    return(out)
}
