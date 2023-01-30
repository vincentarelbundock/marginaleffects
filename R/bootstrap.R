bootstrap_boot <- function(model, FUN, ...) {
    insight::check_if_installed("boot")

    # attached by `inferences()`
    conf_type <- attr(model, "conf_type")
    checkmate::assert_choice(conf_type, choices = c("perc", "norm", "basic", "bca"))

    # remove the special class so that {marginaleffects} trigger model-approrimate
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