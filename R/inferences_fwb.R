inferences_fwb <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", mfx = NULL, ...) {
    insight::check_if_installed("fwb", minimum_version = "0.5.0")

    out <- x
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

    out$std.error <- fwb_summary$std.error
    out$conf.low <- fwb_summary$conf.low
    out$conf.high <- fwb_summary$conf.high

    cols <- setdiff(names(out), "df")

    if ("p.value" %in% names(fwb_summary)) {
        out$p.value <- fwb_summary$p.value
        out$s.value <- -log2(out$p.value)
    } else {
        cols <- setdiff(cols, c("s.value", "p.value"))
    }

    if ("statistic" %in% names(fwb_summary)) {
        out$statistic <- fwb_summary$statistic
    } else {
        cols <- setdiff(cols, "statistic")
    }

    out <- out[, cols, drop = FALSE]

    attr(out, "inferences") <- B
    attr(out, "posterior_draws") <- t(B$t)
    return(out)
}
