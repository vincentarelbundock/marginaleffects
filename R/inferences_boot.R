inferences_boot <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", estimator = NULL, ...) {
    out <- x
    call_mfx <- attr(x, "call")
    call_mfx[["vcov"]] <- FALSE
    modeldata <- call_mfx[["modeldata"]]
    if (is.null(modeldata)) {
        modeldata <- get_modeldata(call_mfx[["model"]])
    }
    if (!is.null(estimator)) {
        bootfun <- function(data, indices) {
            d <- data[indices, , drop = FALSE]
            boot_mfx <- estimator(d)
            return(boot_mfx$estimate)
        }
    } else {
        bootfun <- function(data, indices) {
            d <- data[indices, , drop = FALSE]
            call_mod <- insight::get_call(call_mfx[["model"]])
            call_mod[["data"]] <- d
            boot_mod <- eval.parent(call_mod)
            call_mfx[["model"]] <- boot_mod
            call_mfx[["modeldata"]] <- d
            boot_mfx <- eval.parent(call_mfx)
            return(boot_mfx$estimate)
        }
    }

    args <- list("data" = modeldata, "statistic" = bootfun, R = R)
    B <- do.call(boot::boot, args)

    # print.boot prints an ugly nested call
    t <- matrix(B$t, nrow = nrow(B$t))
    op <- cbind(
        colMeans(t, na.rm = TRUE),
        sqrt(apply(t, 2L, function(t.st) stats::var(t.st[!is.na(t.st)])))
    )

    # extract from weird boot.ci() list (inspired from `broom::tidy.broom` under MIT)
    ci_list <- lapply(
        seq_along(B$t0),
        boot::boot.ci,
        boot.out = B,
        conf = conf_level,
        type = conf_type
    )
    pos <- pmatch(conf_type, names(ci_list[[1]]))
    if (conf_type == "norm") {
        cols <- 2:3
    } else {
        cols <- 4:5
    }
    ci <- lapply(ci_list, function(x) x[[pos]])
    ci <- do.call("rbind", ci)[, cols]

    # add CI to original {marginaleffects} call
    if (is.matrix(ci)) {
        out$conf.low <- ci[, 1]
        out$conf.high <- ci[, 2]
    } else {
        out$conf.low <- ci[1]
        out$conf.high <- ci[2]
    }

    cols <- setdiff(names(out), c("p.value", "std.error", "statistic", "s.value", "df"))
    out <- out[, cols, drop = FALSE]

    attr(out, "inferences") <- B
    attr(out, "posterior_draws") <- t(B$t)
    return(out)
}
