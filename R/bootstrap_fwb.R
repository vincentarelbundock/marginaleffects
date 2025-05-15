bootstrap_fwb <- function(model, INF_FUN, ...) {
    # attached by `inferences()`
    conf_type <- attr(model, "inferences_conf_type")
    checkmate::assert_choice(
        conf_type,
        choices = c("perc", "norm", "basic", "bc", "bca")
    )

    # bootstrap using the original data and call
    modeldata <- get_modeldata(model, additional_variables = FALSE)

    # evaluate the {marginaleffects} call to get output without inferences()
    # use ... because arguments are not the same for different {marginaleffects} functions
    dots <- list(...)
    dots[["vcov"]] <- FALSE

    # avoid recursion
    attr(model, "inferences_method") <- NULL
    out <- do.call(INF_FUN, c(list(model), dots))

    # default confidence level may be implicit in original call, but we need numeric
    if (is.null(dots[["conf_level"]])) {
        conf_level <- 0.95
    } else {
        conf_level <- dots[["conf_level"]]
    }

    bootfun <- function(data, w) {
        # If model has weights, multiply them by random weights
        if (!is.null(w0 <- stats::weights(model))) w <- w * w0

        # Update the model's call and evaluate
        modboot <- stats::update(model, weights = w, evaluate = TRUE)

        # {marginaleffects} function needs to incorporate weights if
        # averaging. May be a problem if other weights supplied to
        # `wts` argument.
        dots[["wts"]] <- w
        args <- c(list(modboot), dots)
        out <- do.call(INF_FUN, args)$estimate
        return(out)
    }
    args <- list("data" = modeldata, "statistic" = bootfun)
    args <- c(args, attr(model, "inferences_dots"))
    args <- args[unique(names(args))]

    B <- do.call(fwb::fwb, args)

    # print.boot prints an ugly nested call
    B$call <- match.call()

    # Extract SEs and CIs
    fwb_summary <- summary(B, conf = conf_level, ci.type = conf_type)

    out$std.error <- fwb_summary[, "Std. Error"]
    out$conf.low <- fwb_summary[, 3]
    out$conf.high <- fwb_summary[, 4]

    attr(out, "inferences") <- B
    attr(out, "posterior_draws") <- t(B$t)
    return(out)
}
