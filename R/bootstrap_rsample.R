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