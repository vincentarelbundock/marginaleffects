bootstrap_rsample <- function(model, INF_FUN, ...) {
    # attached by `inferences()`
    conf_type <- attr(model, "inferences_conf_type")
    checkmate::assert_choice(conf_type, choices = c("perc", "bca"))

    # attached by `inferences()`
    conf_type <- attr(model, "inferences_conf_type")
    checkmate::assert_choice(
        conf_type,
        choices = c("perc", "norm", "basic", "bca")
    )

    # bootstrap using the original data and call
    modcall <- insight::get_call(model)
    modeldata <- get_modeldata(model, additional_variables = FALSE)
    data.table::setDF(modeldata)

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

    bootfun <- function(data, ...) {
        modcall[["data"]] <- data
        modboot <- eval(modcall)
        modboot <- eval(modboot)
        args <- c(list(modboot, modeldata = data$data), dots)
        out <- do.call(INF_FUN, args)
        out <- tidy(out)
        # `rsample` averages by `term` columns; we don't use it anyway and assume things line up
        out$term <- seq_len(nrow(out))
        return(out)
    }
    args <- attr(model, "inferences_dots")
    args[["data"]] <- modeldata
    args[["apparent"]] <- TRUE # require for "bca"
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
