get_dydx_and_se <- function(model,
                            variables,
                            newdata = insight::get_data(model),
                            vcov = stats::vcov(model),
                            group_name = NULL,
                            type = "response",
                            numDeriv_method = "simple",
                            ...) {

    coefs <- get_coef(model)
    group_names <- get_group_names(model)

    if (!is.null(vcov)) {
        vcov <- vcov[names(coefs), names(coefs), drop = FALSE]
    }

    mfx_list <- list()
    J_mean_list <- list()

    # compute marginal effects and standard errors
    mfx_list <- list()
    se_mean_list <- list()
    draws_list <- list()
    for (predt in type) {
        for (gn in group_names) {
            J_mean_tmp <- list()
            for (variable in variables) {
                mfx <- get_dydx(model = model,
                                variable = variable,
                                group_name = gn,
                                newdata = newdata,
                                type = predt,
                                numDeriv_method = numDeriv_method,
                                ...)
                draws_list <- c(draws_list, list(attr(mfx, "posterior_draws")))
                mfx <- get_dydx_se(model = model,
                                   mfx = mfx,
                                   vcov = vcov,
                                   variable = variable,
                                   group_name = gn,
                                   newdata = newdata,
                                   type = predt,
                                   numDeriv_method = numDeriv_method,
                                   ...)
                mfx$type <- predt
                mfx$group <- gn
                mfx_list <- c(mfx_list, list(mfx))
                J_mean_tmp <- c(J_mean_tmp, list(attr(mfx, "J_mean")))
            }

            if (!is.null(vcov)) {
                # Standard errors at the mean gradient (this is what `Stata` and `margins` report)
                J_mean <- do.call("rbind", J_mean_tmp)
                # J_mean will be NULL in bayesian models and where the delta method breaks
                if (!is.null(J_mean)) {
                    V <- colSums(t(J_mean %*% vcov) * t(J_mean))
                    tmp <- data.frame("group" = gn, 
                                      "type" = predt, 
                                      "term" = names(V), 
                                      "std.error" = sqrt(V))
                    se_mean_list <- c(se_mean_list, list(tmp))
                }
            }
        }
    }

    # could have different columns, so `rbind` won't do
    out <- bind_rows(mfx_list)
    row.names(out) <- NULL

    if (!is.null(vcov)) {
        se <- do.call("rbind", se_mean_list)
        if ("group" %in% colnames(se) && all(se$group == "main_marginaleffect")) {
            se$group <- NULL
        }
        attr(out, "se_at_mean_gradient") <- se
    }

    # bayesian posterior draws
    browser()
    draws <- bind_rows(draws_list)
    if (!is.null(draws)) {
        assert_dependency("bayestestR")
        attr(out, "posterior_draws") <- draws
        if (!"conf.low" %in% colnames(out)) {
            tmp <- bayestestR::hdi(data.frame(t(draws)))
            out[["std.error"]] <- NULL
            out[["dydx"]] <- apply(draws, 1, stats::median)
            out[["conf.low"]] <- tmp$CI_low
            out[["conf.high"]] <- tmp$CI_high
        }
    }

    return(out)
}
