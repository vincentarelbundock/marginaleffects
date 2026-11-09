inferences_fwb <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", mfx = NULL, newdata_supplied = TRUE, ...) {
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

    # The random weights can reweight the averaging only when the estimates are
    # computed over the rows of the model data. Otherwise, the model is refit
    # with the random weights, but the averaging over `newdata` is fixed.
    wts_user <- mfx@wts
    on_modeldata <- !newdata_supplied || fwb_same_rows(mfx@newdata, modeldata)
    if (!on_modeldata) {
        msg <- "`inferences(method = \"fwb\")`: `newdata` is not the data used to fit the model. The bootstrap weights are used to refit the model, but not to reweight the averaging over `newdata`, which is treated as fixed."
        warn_once(msg, "marginaleffects_fwb_newdata_fixed")
        # The call may refer to `wts` by name; use the resolved numeric weights.
        if (!is.null(wts_user) && isTRUE(length(wts_user) == nrow(mfx@newdata))) {
            call_mfx[["wts"]] <- wts_user
        }
    }

    bootfun <- function(data, w, ...) {
        w_fwb <- w
        # If model has weights, multiply them by random weights
        if (!is.null(w0 <- stats::weights(mfx@model))) {
            w <- w * w0
        }

        # Update the model's call and evaluate on `modeldata` (the rows the
        # weights correspond to; NA rows already dropped), in the formula
        # environment so function-local data and variables resolve.
        call_mod <- insight::get_call(mfx@model)
        call_mod[["weights"]] <- w
        if ("data" %in% names(call_mod)) {
            call_mod[["data"]] <- data
        }
        env <- tryCatch(environment(stats::formula(mfx@model)), error = function(e) NULL)
        if (is.null(env)) {
            env <- parent.frame()
        }
        boot_mod <- eval(call_mod, envir = env)

        # Update marginaleffects call
        call_mfx[["model"]] <- boot_mod
        call_mfx[["modeldata"]] <- data
        if (on_modeldata) {
            call_mfx[["wts"]] <- if (is.null(wts_user)) w else w_fwb * wts_user
        }
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

    mfx@draws <- t(B$t)
    mfx@inferences <- B
    attr(out, "marginaleffects") <- mfx

    return(out)
}


# TRUE when `newdata` holds exactly the rows of `modeldata`, in the same order.
fwb_same_rows <- function(newdata, modeldata) {
    if (!inherits(newdata, "data.frame") || !inherits(modeldata, "data.frame")) {
        return(FALSE)
    }
    if (nrow(newdata) != nrow(modeldata)) {
        return(FALSE)
    }
    cols <- setdiff(intersect(colnames(newdata), colnames(modeldata)), "rowid")
    if (length(cols) == 0) {
        return(FALSE)
    }
    for (v in cols) {
        same <- isTRUE(all.equal(newdata[[v]], modeldata[[v]], check.attributes = FALSE))
        if (!same) {
            return(FALSE)
        }
    }
    return(TRUE)
}
