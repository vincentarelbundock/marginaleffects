inferences_simulation <- function(x, R = 1000, conf_level = 0.95, ...) {
    insight::check_if_installed("mvtnorm")

    out <- x
    model <- attr(x, "model")
    call_mfx <- attr(x, "call")
    call_mfx[["vcov"]] <- FALSE

    B <- get_coef(model)

    # respect robust vcov from the first call
    V <- attr(out, "vcov")
    if (!isTRUE(checkmate::check_matrix(V))) {
        V <- get_vcov(model)
    }

    # Draw R sets of coefficients from multivariate normal
    coefmat <- mvtnorm::rmvnorm(R, B, V)

    # avoid recursion
    args <- list(...)
    args[["vcov"]] <- FALSE

    # Function to compute estimates with simulated coefficients
    inner_fun <- function(i = NULL) {
        mod_tmp <- set_coef(model, coefmat[i, ])
        call_mfx[["model"]] <- mod_tmp
        boot_mfx <- recall(call_mfx)
        return(boot_mfx$estimate)
    }

    # Compute estimates for each set of coefficients
    # do not use simulation mean as point estimate
    # https://doi.org/10.1017/psrm.2023.8
    draws <- lapply(seq_len(nrow(coefmat)), inner_fun)
    draws <- do.call("cbind", draws)

    # Compute confidence intervals
    out$std.error <- apply(draws, 1, stats::sd)
    alpha <- 1 - conf_level
    out$conf.low <- apply(draws, 1, stats::quantile, probs = alpha / 2)
    out$conf.high <- apply(draws, 1, stats::quantile, probs = 1 - alpha / 2)

    # Drop unnecessary columns
    cols <- setdiff(names(out), c("p.value", "std.error", "statistic", "s.value", "df"))
    out <- out[, cols, drop = FALSE]

    attr(out, "posterior_draws") <- draws
    return(out)
}
