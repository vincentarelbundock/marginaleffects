#' Bootstrap rsample v2
#' @export
bootstrap_rsample2 <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", ...) {
    out <- x
    call_mfx <- attr(x, "call")
    call_mfx[["vcov"]] <- FALSE
    modeldata <- call_mfx[["modeldata"]]
    if (is.null(modeldata)) {
        modeldata <- get_modeldata(call_mfx[["model"]])
    }

    bootfun <- function(split, ...) {
        d <- rsample::analysis(split)
        call_mod <- insight::get_call(call_mfx[["model"]])
        call_mod[["data"]] <- d
        boot_mod <- evalup(call_mod)
        call_mfx[["model"]] <- boot_mod
        call_mfx[["modeldata"]] <- d
        boot_mfx <- evalup(call_mfx)
        out <- tidy(boot_mfx)
        out$term <- seq_len(nrow(out))
        return(out)
    }

    args <- list("data" = modeldata, "apparent" = TRUE)
    args[["times"]] <- R
    splits <- do.call(rsample::bootstraps, args)
    splits$estimates <- lapply(splits$splits, bootfun)

    if (isTRUE(conf_type == "bca")) {
        ci <- rsample::int_bca(
            splits,
            statistics = estimates,
            .fn = bootfun,
            alpha = 1 - conf_level
        )
    } else {
        ci <- rsample::int_pctl(
            splits,
            statistics = estimates,
            alpha = 1 - conf_level
        )
    }

    out$conf.low <- ci$.lower
    out$conf.high <- ci$.upper

    cols <- setdiff(names(out), c("p.value", "std.error", "statistic", "s.value", "df"))
    out <- out[, cols, drop = FALSE]

    attr(out, "inferences") <- splits
    draws <- lapply(
        splits$estimates,
        function(x) as.matrix(x[, "estimate", drop = FALSE])
    )
    draws[[length(draws)]] <- NULL # apparent=TRUE appended the original estimates to the end
    draws <- do.call("cbind", draws)
    colnames(draws) <- NULL
    attr(out, "posterior_draws") <- draws
    return(out)
}
