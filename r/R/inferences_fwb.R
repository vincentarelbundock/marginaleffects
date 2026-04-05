inferences_fwb <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", mfx = NULL, ...) {
    insight::check_if_installed("fwb", minimum_version = "0.5.0")

    call_mfx <- mfx@call
    call_mfx[["vcov"]] <- FALSE

    # Get modeldata from mfx object
    modeldata <- mfx@modeldata

    # Ensure parameters are embedded in the call, not just references
    if (!is.null(mfx@newdata)) {
        call_mfx[["newdata"]] <- mfx@newdata
    }
    if (!is.null(mfx@comparison)) {
        call_mfx[["comparison"]] <- mfx@comparison
    }

    bootfun <- function(data, w, ...) {
        # If model has weights, multiply them by random weights
        if (!is.null(w0 <- stats::weights(mfx@model))) {
            w <- w * w0
        }

        # Update the model's call and evaluate
        call_mod <- insight::get_call(mfx@model)
        call_mod[["weights"]] <- w
        boot_mod <- eval.parent(call_mod)

        # Update marginaleffects call
        call_mfx[["model"]] <- boot_mod
        call_mfx[["modeldata"]] <- data
        call_mfx[["wts"]] <- w
        boot_mfx <- eval.parent(call_mfx)
        return(boot_mfx$estimate)
    }

    args <- list("data" = modeldata, "statistic" = bootfun, R = R)
    args <- c(args, list(...))

    # fwb default verbose is TRUE
    if (!"verbose" %in% names(args)) {
        args[["verbose"]] <- FALSE
    }

    if (
        isTRUE(getOption("marginaleffects_parallel_inferences", default = FALSE)) &&
            !"cl" %in% names(args)
    ) {
        args[["cl"]] <- "future"
    }

    B <- do.call(fwb::fwb, args)

    # Extract SEs and CIs
    fwb_summary <- tidy(summary(B, conf = conf_level, ci.type = conf_type, p.value = TRUE))

    p_val <- if ("p.value" %in% names(fwb_summary)) fwb_summary$p.value else NULL
    stat <- if ("statistic" %in% names(fwb_summary)) fwb_summary$statistic else NULL
    s_val <- if (!is.null(p_val)) -log2(p_val) else NULL

    list(
        conf.low = fwb_summary$conf.low,
        conf.high = fwb_summary$conf.high,
        std.error = fwb_summary$std.error,
        p.value = p_val,
        statistic = stat,
        s.value = s_val,
        draws = t(B$t),
        inferences_object = B
    )
}
