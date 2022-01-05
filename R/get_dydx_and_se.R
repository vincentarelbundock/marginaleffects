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
    J_list <- list()
    J_mean_list <- list()
    for (predt in type) {
        for (variable in variables) {
            mfx <- get_dydx(model = model,
                            variable = variable,
                            newdata = newdata,
                            type = predt,
                            ...)
            mfx$type <- predt

            # bayesian draws
            if (!is.null(attr(mfx, "posterior_draws"))) {
                draws_list <- c(draws_list, list(attr(mfx, "posterior_draws")))
                J <- J_mean <- NULL
            # standard errors via delta method
            } else if (!is.null(vcov)) {
                idx <- intersect(colnames(mfx), c("group", "term", "contrast"))
                idx <- mfx[, idx, drop = FALSE]
                se <- delta_se(model,
                               vcov = vcov,
                               type = predt,
                               FUN = delta_se_marginaleffects,
                               newdata = newdata,
                               index = idx,
                               variable = variable)
                mfx$std.error <- as.numeric(se)
                J <- attr(se, "J")
                J_mean <- attr(se, "J_mean")
            } else {
                J <- J_mean <- NULL
            }
            mfx_list <- c(mfx_list, list(mfx))
            J_list <- c(J_list, list(J))
            J_mean_list <- c(J_mean_list, list(J_mean))

        }
    }

    # could have different columns, so `rbind` won't do
    out <- bind_rows(mfx_list)
    row.names(out) <- NULL

    J <- do.call("rbind", J_list) # bind_rows does not work for matrices
    J_mean <- bind_rows(J_mean_list) # bind_rows need because some have contrast col

    # empty contrasts equal "". important for merging in `tidy()`
    if ("contrast" %in% colnames(J_mean)) {
        J_mean$contrast <- ifelse(is.na(J_mean$contrast), "", J_mean$contrast)
    }

    # standard error at mean gradient
    # J_mean is NULL in bayesian models and where the delta method breaks
    if (!is.null(J_mean) && !is.null(vcov)) {
        idx <- !colnames(J_mean) %in% c("group", "term", "contrast")
        tmp <- J_mean[, !idx, drop = FALSE]
        J_mean_mat <- as.matrix(J_mean[, idx, drop = FALSE])
        # converting to data.frame can sometimes break colnames
        colnames(J_mean_mat) <- colnames(J)
        # aggressive check. probably needs to be relaxed.
        if (any(colnames(J_mean_mat) != colnames(vcov))) {
            browser()
            tmp <- NULL
            warning("The variance covariance matrix and the Jacobian do not match. `marginaleffects` is unable to compute standard errors using the delta method.")
        } else {
            V <- colSums(t(J_mean_mat %*% vcov) * t(J_mean_mat))
            tmp$std.error <- sqrt(V)
        }
        se_at_mean_gradient <- tmp
    } else {
        se_at_mean_gradient <- NULL
    }

    # attributes
    attr(out, "J") <- J
    attr(out, "J_mean") <- J_mean
    attr(out, "se_at_mean_gradient") <- se_at_mean_gradient

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

    return(out)
}
