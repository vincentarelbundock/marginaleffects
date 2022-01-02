get_dydx_and_se <- function(model,
                            variables,
                            newdata = insight::get_data(model),
                            vcov = stats::vcov(model),
                            type = "response",
                            numDeriv_method = "simple",
                            ...) {

    coefs <- get_coef(model)

    # Needed for `AER::tobit` and others where `vcov` includes Log(scale) but `coef` does not
    # Dangerous for `oridinal::clm` and others where there are important duplicate column names
    # in `vcov`, and selecting with [,] repeats the first instance.
    if (anyDuplicated(names(vcov)) == 0) {
        if (!is.null(vcov) && all(names(coefs) %in% colnames(vcov))) {
            vcov <- vcov[names(coefs), names(coefs), drop = FALSE]
        }
    }

    mfx_list <- list()

    # compute marginal effects and standard errors
    mfx_list <- list()
    se_mean_list <- list()
    draws_list <- list()
    for (predt in type) {
        for (variable in variables) {
            mfx <- get_dydx(model = model,
                            variable = variable,
                            newdata = newdata,
                            type = predt,
                            numDeriv_method = numDeriv_method,
                            ...)

            if (!is.null(attr(mfx, "posterior_draws"))) {
                draws_list <- c(draws_list, list(attr(mfx, "posterior_draws")))
            } else {
                mfx <- get_dydx_se(model = model,
                                   mfx = mfx,
                                   vcov = vcov,
                                   variable = variable,
                                   newdata = newdata,
                                   type = predt,
                                   numDeriv_method = numDeriv_method,
                                   ...)
            }

            mfx$type <- predt
            mfx_list <- c(mfx_list, list(mfx))

            J_mean <- attr(mfx, "J_mean")
            if (!is.null(J_mean)) {
                idx <- !colnames(J_mean) %in% c("group", "term", "contrast")
                J_mean_mat <- J_mean[, idx, drop = FALSE]
                J_mean_mat <- as.matrix(J_mean_mat)
                # J_mean is NULL in bayesian models and where the delta method breaks
                if (!is.null(vcov) && !is.null(J_mean)) {
                    V <- colSums(t(J_mean_mat %*% vcov) * t(J_mean_mat))
                    tmp <- J_mean[, intersect(colnames(J_mean), c("group", "term", "contrast")), drop = FALSE]
                    if (!"contrast" %in% colnames(tmp)) {
                        tmp$contrast <- ""
                    }
                    tmp$type <- predt
                    tmp$std.error <- sqrt(V)
                    se_mean_list <- c(se_mean_list, list(tmp))
                }
            }
        }
    }

    # could have different columns, so `rbind` won't do
    out <- bind_rows(mfx_list)
    row.names(out) <- NULL

    # bayesian posterior draws
    draws <- do.call("rbind", draws_list)
    if (!is.null(draws)) {
        attr(out, "posterior_draws") <- draws
        if (!"conf.low" %in% colnames(out)) {
            tmp <- apply(draws, 1, get_hdi)
            out[["std.error"]] <- NULL
            out[["dydx"]] <- apply(draws, 1, stats::median)
            out[["conf.low"]] <- tmp[1, ]
            out[["conf.high"]] <- tmp[2, ]
        }
    }

    # standard errors
    if (!is.null(vcov)) {
        # group: outcome level
        # term: variable
        se <- bind_rows(se_mean_list)
        if ("group" %in% colnames(se) && all(se$group == "main_marginaleffect")) {
            se$group <- NULL
        }
        attr(out, "se_at_mean_gradient") <- se
    }

    return(out)
}
