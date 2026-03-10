inferences_simulation <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", mfx = NULL, ...) {
    insight::check_if_installed("mvtnorm")

    checkmate::assert_choice(
        conf_type,
        choices = c(
            "perc",
            "wald"
        )
    )

    out <- x
    model <- mfx@model
    call_mfx <- mfx@call
    call_mfx[["vcov"]] <- FALSE
    # avoid calling get_modeldata() repeatedly via ...
    call_mfx[["modeldata"]] <- mfx@modeldata

    B <- get_coef(model)

    # respect robust vcov from the first call
    V <- mfx@vcov_model
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
        boot_mfx <- eval.parent(call_mfx)
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

    if (conf_type == "perc") {
        out$conf.low <- apply(draws, 1, stats::quantile, probs = alpha / 2, names = FALSE)
        out$conf.high <- apply(draws, 1, stats::quantile, probs = 1 - alpha / 2, names = FALSE)

        cols <- setdiff(names(out), c("p.value", "std.error", "statistic", "s.value", "df"))
    } else if (conf_type == "wald") {
        out$statistic <- out$estimate / out$std.error

        critical <- abs(stats::qnorm(alpha / 2))

        out$conf.low <- out$estimate - critical * out$std.error
        out$conf.high <- out$estimate + critical * out$std.error

        out$p.value <- 2 * stats::pnorm(-abs(out$statistic))
        out$s.value <- -log2(out$p.value)

        cols <- setdiff(names(out), "df")
    }

    # Drop unnecessary columns
    out <- out[, cols, drop = FALSE]

    mfx@draws <- draws
    attr(out, "marginaleffects") <- mfx

    return(out)
}
