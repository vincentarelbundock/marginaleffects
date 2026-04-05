inferences_boot <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", estimator = NULL, data_train = NULL, mfx = NULL, ...) {
    insight::check_if_installed("boot")

    if (!is.null(estimator)) {
        bootfun <- function(data, indices) {
            d <- data[indices, , drop = FALSE]
            boot_mfx <- estimator(d)
            return(boot_mfx$estimate)
        }
    } else {
        bootfun <- function(data, indices) {
            d <- data[indices, , drop = FALSE]
            boot_mfx <- refit(x, data = d, vcov = FALSE)
            return(boot_mfx$estimate)
        }
    }

    args <- list("data" = data_train, "statistic" = bootfun, R = R)
    args <- c(args, list(...))
    B <- do.call(boot::boot, args)

    # print.boot prints an ugly nested call
    t <- matrix(B$t, nrow = nrow(B$t))
    op <- cbind(
        colMeans(t, na.rm = TRUE),
        sqrt(apply(t, 2L, function(t.st) stats::var(t.st[!is.na(t.st)])))
    )

    # extract from weird boot.ci() list (inspired from `broom::tidy.broom` under MIT)
    ci_list <- suppressWarnings(lapply(
        seq_along(B$t0),
        boot::boot.ci,
        boot.out = B,
        conf = conf_level,
        type = conf_type
    )) # extreme order statistics
    pos <- pmatch(conf_type, names(ci_list[[1]]))
    if (conf_type == "norm") {
        cols <- 2:3
    } else {
        cols <- 4:5
    }
    ci <- lapply(ci_list, function(x) x[[pos]])
    ci <- do.call("rbind", ci)[, cols]

    if (is.matrix(ci)) {
        conf_low <- ci[, 1]
        conf_high <- ci[, 2]
    } else {
        conf_low <- ci[1]
        conf_high <- ci[2]
    }

    list(
        conf.low = conf_low,
        conf.high = conf_high,
        std.error = NULL,
        p.value = NULL,
        statistic = NULL,
        s.value = NULL,
        draws = t(B$t),
        inferences_object = B
    )
}
