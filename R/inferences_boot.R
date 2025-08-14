inferences_boot <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", estimator = NULL, mfx = NULL, ...) {
    insight::check_if_installed("boot")

    out <- x
    call_mfx <- mfx@call
    call_mfx[["vcov"]] <- FALSE

    if (!is.null(estimator)) {
        bootfun <- function(data, indices) {
            d <- data[indices, , drop = FALSE]
            boot_mfx <- estimator(d)
            return(boot_mfx$estimate)
        }
    } else {
        bootfun <- function(data, indices) {
            d <- data[indices, , drop = FALSE]
            call_mod <- insight::get_call(mfx@model)
            call_mod[["data"]] <- d
            boot_mod <- eval.parent(call_mod)
            call_mfx <- mfx@call
            call_mfx[["model"]] <- boot_mod
            boot_mfx <- eval.parent(call_mfx)
            return(boot_mfx$estimate)
        }
    }

    args <- list("data" = mfx@modeldata, "statistic" = bootfun, R = R)
    args <- c(args, list(...))
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

    mfx <- attr(x, "mfx")
    mfx@draws <- t(B$t)
    mfx@inferences <- B
    attr(out, "mfx") <- mfx

    return(out)
}
