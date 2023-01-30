bootstrap_dispatch <- function(model, FUN, ...) {
    if (inherits(model, "inferences_rsample")) {
        bootstrap_rsample(model = model, FUN = FUN, ...)
    } else if (inherits(model, "inferences_boot")) {
        bootstrap_boot(model = model, FUN = FUN, ...)
    }
}


bootstrap_rsample <- function(model, FUN, ...) {
    conf_type <- attr(model, "conf_type")
    checkmate::assert_choice(conf_type, choices = c("perc", "bca"))
    insight::check_if_installed("dplyr")
    insight::check_if_installed("rsample")
    class(model) <- setdiff(class(model), "inferences_rsample")
    modeldata <- get_modeldata(model)
    modcall <- insight::get_call(model)
    data.table::setDF(modeldata)
    dots <- list(...)
    dots[["vcov"]] <- FALSE
    out <- do.call(FUN, c(list(model), dots))
    if (is.null(dots[["conf_level"]])) {
        conf_level <- 0.95
    } else {
        conf_level <- dots[["conf_level"]]
    }
    bootfun <- function(data, ...) {
        modcall[["data"]] <- data
        modboot <- eval(modcall)
        modboot <- eval(modboot)
        args <- c(list(modboot), dots)
        out <- do.call(FUN, args)
        out <- tidy(out)
        # `rsample` averages by `term` columns; we don't use it anyway and assume things line up
        out$term <- seq_len(nrow(out))
        return(out)
    }
    args <- attr(model, "boot_args")
    args[["data"]] <- modeldata
    args[["apparent"]] <- TRUE
    splits <- do.call(rsample::bootstraps, args)
    splits$estimates <- lapply(splits$splits, bootfun)

    if (isTRUE(conf_type == "bca")) {
        ci <- rsample::int_bca(
            splits,
            statistics = estimates,
            .fn = bootfun,
            alpha = 1 - conf_level)
    } else {
        ci <- rsample::int_pctl(
            splits,
            statistics = estimates,
            alpha = 1 - conf_level)
    }

    out$conf.low <- ci$.lower
    out$conf.high <- ci$.upper

    draws <- lapply(splits$estimates, function(x) as.matrix(x[, "estimate", drop = FALSE]))
    draws[[length(draws)]] <- NULL # apparent=TRUE appended the original estimates to the end
    draws <- do.call("cbind", draws)
    colnames(draws) <- NULL
    attr(out, "posterior_draws") <- draws
    attr(out, "rsample") <- splits
    return(out)
}


bootstrap_boot <- function(model, FUN, ...) {
    conf_type <- attr(model, "conf_type")
    checkmate::assert_choice(conf_type, choices = c("perc", "norm", "basic", "bca"))
    insight::check_if_installed("boot")
    insight::check_if_installed("broom")
    class(model) <- setdiff(class(model), "inferences_boot")
    modeldata <- get_modeldata(model)
    data.table::setDF(modeldata)
    modcall <- insight::get_call(model)
    dots <- list(...)
    dots[["vcov"]] <- FALSE
    out <- do.call(FUN, c(list(model), dots))
    bootfun <- function(data, indices) {
        d <- data[indices, , drop = FALSE]
        modcall[["data"]] <- d
        modboot <- eval(modcall)
        modboot <- eval(modboot)
        args <- c(list(modboot), dots)
        out <- do.call(FUN, args)$estimate
        return(out)
    }
    # no-bootstrap object to return
    if (is.null(dots[["conf_level"]])) {
        conf_level <- 0.95
    } else {
        conf_level <- dots[["conf_level"]]
    }
    args <- list("data" = modeldata, "statistic" = bootfun)
    args <- c(args, attr(model, "boot_args"))
    args <- args[unique(names(args))]
    boot::boot(args$data, args$statistic, args$R)
    B <- do.call(boot::boot, args)
    B$call <- match.call()

    # HACK: parse boot() print
    pr <- utils::capture.output(print(B))
    pr <- pr[(grep("^Bootstrap Statistics :", pr) + 1):length(pr)]
    pr <- gsub("std. error", "std.error", pr)
    pr <- paste(pr, collapse = "\n")
    pr <- utils::read.table(text = pr, header = TRUE)
    out$std.error <- pr$std.error

    # extract from weird boot.ci() list (taken from `broom` -- MIT)
    ci_list <- lapply(seq_along(B$t0),
                      boot::boot.ci,
                      boot.out = B,
                      conf = conf_level, 
                      type = conf_type)
    pos <- pmatch(conf_type, names(ci_list[[1]]))
    if (conf_type == "norm") {
        cols <- 2:3
    } else {
        cols <- 4:5
    }
    ci <- lapply(ci_list, function(x) x[[pos]])
    ci <- do.call("rbind", ci)[, cols]
    if (is.matrix(ci)) {
        out$conf.low <- ci[, 1]
        out$conf.high <- ci[, 2]
    } else {
        out$conf.low <- ci[1]
        out$conf.high <- ci[2]
    }
    attr(out, "boot") <- B
    attr(out, "posterior_draws") <- t(B$t)
    return(out)
}