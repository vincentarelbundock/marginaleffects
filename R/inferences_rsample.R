inferences_rsample <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", estimator = NULL, ...) {
    insight::check_if_installed("rsample")

    out <- x
    call_mfx <- attr(x, "call")
    call_mfx[["vcov"]] <- FALSE
    modeldata <- call_mfx[["modeldata"]]
    if (is.null(modeldata)) {
        modeldata <- get_modeldata(call_mfx[["model"]])
    }

    if (!is.null(estimator)) {
        bootfun <- function(split, ...) {
            d <- rsample::analysis(split)
            result <- estimator(d)
            return(result)
        }
    } else {
        bootfun <- function(split, ...) {
            d <- rsample::analysis(split)
            call_mod <- insight::get_call(call_mfx[["model"]])
            call_mod[["data"]] <- d
            boot_mod <- eval.parent(call_mod)
            call_mfx[["model"]] <- boot_mod
            call_mfx[["modeldata"]] <- d
            boot_mfx <- eval.parent(call_mfx)
            out <- data.frame(boot_mfx)
            return(out)
        }
    }

    # rsample::int_bca/pctl collapse estimates when term is duplicated because of term/contrast/by unique
    bootfun_term <- function(split, ...) {
        out <- bootfun(split, ...)
        # This is a hack to avoid the issue of rsample::int_bca/pctl collapsing estimates when term is duplicated because of term/contrast/by unique
        # Warning: assumes that we always return estimates in the same order as the original {marginaleffects} call.
        # data.frame() to remove super heavy attributes (model, data, etc.)
        # as.character() because `rsample` assumes character `term`. Reported here:
        # https://github.com/tidymodels/rsample/issues/574
        out <- data.frame(
            term = as.character(seq_len(nrow(out))),
            estimate = out$estimate
        )
        return(out)
    }

    args <- list("data" = modeldata, "apparent" = TRUE)
    args[["times"]] <- R
    args <- c(args, list(...))
    splits <- do.call(rsample::bootstraps, args)
    if (isTRUE(getOption("marginaleffects_parallel_inferences", default = FALSE))) {
        insight::check_if_installed("future.apply")
        pkg <- getOption("marginaleffects_parallel_packages", default = NULL)
        pkg <- unique(c("marginaleffects", pkg))
        splits$results <- future.apply::future_lapply(
            splits$splits,
            bootfun_term,
            future.seed = TRUE,
            future.packages = pkg
        )
    } else {
        splits$results <- lapply(splits$splits, bootfun_term)
    }

    if (isTRUE(conf_type == "bca")) {
        ci <- rsample::int_bca(
            splits,
            statistics = results,
            .fn = bootfun_term,
            alpha = 1 - conf_level
        )
    } else {
        ci <- rsample::int_pctl(
            splits,
            statistics = results,
            alpha = 1 - conf_level
        )
    }

    # hack: rsample only supports character `term`
    # https://github.com/tidymodels/rsample/issues/574
    ci$term <- as.numeric(ci$term)
    ci <- ci[order(ci$term), ]

    out$conf.low <- ci$.lower
    out$conf.high <- ci$.upper

    cols <- setdiff(names(out), c("p.value", "std.error", "statistic", "s.value", "df"))
    out <- out[, cols, drop = FALSE]

    if (all(out$term == seq_len(nrow(out)))) {
        out$term <- NULL
    }

    attr(out, "inferences") <- splits
    draws <- lapply(
        splits$results,
        function(x) as.matrix(x[, "estimate", drop = FALSE])
    )
    draws[[length(draws)]] <- NULL # apparent=TRUE appended the original estimates to the end
    draws <- do.call("cbind", draws)
    colnames(draws) <- NULL
    attr(out, "posterior_draws") <- draws
    return(out)
}
