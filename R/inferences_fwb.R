#' Bootstrap fwb v2
#' @export
inferences_fwb <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", ...) {
    out <- x
    call_mfx <- attr(x, "call")
    call_mfx[["vcov"]] <- FALSE
    modeldata <- call_mfx[["modeldata"]]
    if (is.null(modeldata)) {
        modeldata <- get_modeldata(call_mfx[["model"]])
    }

    bootfun <- function(data, w) {
        # If model has weights, multiply them by random weights
        if (!is.null(w0 <- stats::weights(call_mfx[["model"]]))) {
            w <- w * w0
        }

        # Update the model's call and evaluate
        call_mod <- insight::get_call(call_mfx[["model"]])
        call_mod[["weights"]] <- w
        boot_mod <- evalup(call_mod)

        # Update marginaleffects call
        call_mfx[["model"]] <- boot_mod
        call_mfx[["modeldata"]] <- data
        call_mfx[["wts"]] <- w
        boot_mfx <- evalup(call_mfx)
        return(boot_mfx$estimate)
    }

    args <- list("data" = modeldata, "statistic" = bootfun, R = R, verbose = FALSE)
    B <- do.call(fwb::fwb, args)

    # Extract SEs and CIs
    fwb_summary <- summary(B, conf = conf_level, ci.type = conf_type)

    out$std.error <- fwb_summary[, "Std. Error"]
    out$conf.low <- fwb_summary[, 3]
    out$conf.high <- fwb_summary[, 4]

    cols <- setdiff(names(out), c("p.value", "std.error", "statistic", "s.value", "df"))
    out <- out[, cols, drop = FALSE]

    attr(out, "inferences") <- B
    attr(out, "posterior_draws") <- t(B$t)
    return(out)
}
