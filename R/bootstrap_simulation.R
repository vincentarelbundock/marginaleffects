bootstrap_simulation <- function(model, INF_FUN, ...) {
    insight::check_if_installed("MASS")

    dots <- list(...)

    B <- get_coef(model)
    V <- get_vcov(model, ...get("vcov"))
    R <- attr(model, "inferences_R")
    coefmat <- MASS::mvrnorm(R, mu = B, Sigma = V)

    # avoid recursion
    args <- dots
    args[["vcov"]] <- FALSE
    attr(model, "inferences_method") <- NULL

    delta <- do.call(INF_FUN, c(list(model), args))

    inner_fun <- function(i = NULL) {
        args <- dots
        mod_tmp <- set_coef(model, coefmat[i, ])
        attr(mod_tmp, "inferences_method") <- NULL
        args[["model"]] <- mod_tmp
        args[["vcov"]] <- FALSE
        do.call(INF_FUN, args)$estimate
    }

    draws <- lapply(seq_len(nrow(coefmat)), inner_fun)
    draws <- do.call("cbind", draws)

    out <- get_ci(delta, draws = draws, conf_level = args$conf_level)

    # do not use simulation mean as point estimate
    # https://doi.org/10.1017/psrm.2023.8
    out$estimate <- delta$estimate

    attr(out, "posterior_draws") <- draws

    return(out)
}
