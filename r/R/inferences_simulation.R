inferences_simulation_summarize <- function(draws, conf_level, interval = FALSE) {
    alpha <- 1 - conf_level

    # Base quantile errors on missing draws. Keep that behavior in the fallback.
    use_collapse <- requireNamespace("collapse", quietly = TRUE) && !anyNA(draws)

    if (use_collapse) {
        std.error <- collapse::dapply(
            draws,
            MARGIN = 1,
            FUN = collapse::fsd
        )
        if (isTRUE(interval)) {
            ci <- collapse::dapply(
                draws,
                MARGIN = 1,
                FUN = collapse::fquantile,
                probs = c(alpha / 2, 1 - alpha / 2)
            )
            conf.low <- ci[, 1]
            conf.high <- ci[, 2]
        } else {
            conf.low <- conf.high <- NULL
        }
    } else {
        std.error <- apply(draws, 1, stats::sd)
        if (isTRUE(interval)) {
            conf.low <- apply(
                draws,
                1,
                stats::quantile,
                probs = alpha / 2,
                names = FALSE
            )
            conf.high <- apply(
                draws,
                1,
                stats::quantile,
                probs = 1 - alpha / 2,
                names = FALSE
            )
        } else {
            conf.low <- conf.high <- NULL
        }
    }

    list(
        std.error = as.vector(std.error),
        conf.low = as.vector(conf.low),
        conf.high = as.vector(conf.high)
    )
}


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

    # Plan discovery and validation may call model-specific prediction methods.
    # Preserve the RNG state so stochastic prediction methods see the same
    # sequence they would have seen before replay plans were introduced.
    seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (isTRUE(seed_exists)) {
        seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }

    replay <- attr(mfx, "marginaleffects_simulation_replay")

    # Objects passed to inferences() directly do not retain large replay plans.
    # Rebuild the original estimates once and capture a temporary plan.
    if (is.null(replay)) {
        old_capture <- settings_get("marginaleffects_capture_simulation_replay")
        settings_set("marginaleffects_capture_simulation_replay", TRUE)
        replay_out <- tryCatch(
            eval.parent(call_mfx),
            error = function(e) NULL,
            finally = {
                if (is.null(old_capture)) {
                    settings_rm("marginaleffects_capture_simulation_replay")
                } else {
                    settings_set("marginaleffects_capture_simulation_replay", old_capture)
                }
            }
        )
        if (inherits(replay_out, "data.frame")) {
            replay_mfx <- attr(replay_out, "marginaleffects")
            if (inherits(replay_mfx, "marginaleffects_internal")) {
                replay <- attr(replay_mfx, "marginaleffects_simulation_replay")
            }
        }
    }
    replay <- simulation_replay_validate(replay, model, out$estimate)
    if (isTRUE(seed_exists)) {
        assign(".Random.seed", seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
    }

    # Compute estimates for each set of coefficients
    # do not use simulation mean as point estimate
    # https://doi.org/10.1017/psrm.2023.8
    draws <- matrix(NA_real_, nrow = nrow(out), ncol = nrow(coefmat))
    for (i in seq_len(nrow(coefmat))) {
        mod_tmp <- set_coef(model, coefmat[i, ])
        if (is.null(replay)) {
            call_mfx[["model"]] <- mod_tmp
            estimate <- eval.parent(call_mfx)$estimate
        } else {
            estimate <- simulation_replay_evaluate(replay, mod_tmp)
        }
        if (length(estimate) != nrow(draws)) {
            stop_sprintf("Simulation draw changed the number of estimates.")
        }
        draws[, i] <- estimate
    }

    # Compute standard errors and confidence intervals
    draw_summary <- inferences_simulation_summarize(
        draws,
        conf_level = conf_level,
        interval = conf_type == "perc"
    )
    out$std.error <- draw_summary$std.error

    if (conf_type == "perc") {
        out$conf.low <- draw_summary$conf.low
        out$conf.high <- draw_summary$conf.high

        cols <- setdiff(names(out), c("p.value", "std.error", "statistic", "s.value", "df"))
    } else if (conf_type == "wald") {
        alpha <- 1 - conf_level
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
    attr(mfx, "marginaleffects_simulation_replay") <- NULL
    attr(out, "marginaleffects") <- mfx

    return(out)
}
