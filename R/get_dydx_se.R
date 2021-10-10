get_dydx_se <- function(model,
                        mfx,
                        vcov,
                        variable,
                        group_name,
                        fitfram,
                        type,
                        numDeriv_method) {

    coefs <- get_coef(model)

    if (is.null(vcov) ||
        !all(names(coefs) %in% colnames(vcov)) ||
        !all(colnames(vcov) %in% names(coefs))) {
        return(mfx)
    }

    vcov <- vcov[names(coefs), names(coefs), drop = FALSE]

    out <- mfx

    if (variable %in% find_categorical(fitfram)) {
        dydx_fun <- get_dydx_categorical
    } else {
        dydx_fun <- get_dydx_continuous
    }

    inner <- function(x) {
        model_tmp <- set_coef(model, stats::setNames(x, names(coefs)))
        g <- dydx_fun(model = model_tmp,
                      fitfram = fitfram,
                      variable = variable,
                      group_name = group_name,
                      type = type,
                      numDeriv_method = numDeriv_method)
        return(g$dydx)
    }

    J <- numDeriv::jacobian(func = inner,
                            x = coefs,
                            method = numDeriv_method)
    colnames(J) <- names(get_coef(model))
    J_mean <- stats::aggregate(J, by = list(out$term), mean)
    row.names(J_mean) <- J_mean[[1]]
    J_mean[[1]] <- NULL
    J_mean <- as.matrix(J_mean)

    # Unit-level standard errors are much slower to compute (are they, though?)
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
