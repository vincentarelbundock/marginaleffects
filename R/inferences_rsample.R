inferences_rsample <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", estimator = NULL, ...) {
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
            # Validate output
            if (!inherits(result, c("hypotheses", "predictions", "slopes", "comparisons"))) {
                stop_sprintf(
                    "The `estimator` function must return an object of class 'hypotheses', 'predictions', 'slopes', or 'comparisons', but it returned an object of class: %s",
                    paste(class(result), collapse = ", ")
                )
            }
            if (!"term" %in% colnames(result)) {
                result$term <- as.character(seq_len(nrow(result)))
            }
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
            out <- tidy(boot_mfx)
            if (!"term" %in% colnames(out)) {
                out$term <- as.character(seq_len(nrow(out)))
            }
            return(out)
        }
    }

    # rsample::int_bca/pctl collapse estimates when term is duplicated because of term/contrast/by unique
    bootfun_term <- function(split, ...) {
        out <- bootfun(split, ...)
        out$term <- paste(out$term, paste0("marginaleffects", seq_len(nrow(out))))
        # data.frame() to remove super heavy attributes (model, data, etc.)
        return(data.frame(out))
    }

    args <- list("data" = modeldata, "apparent" = TRUE)
    args[["times"]] <- R
    args <- c(args, list(...))
    splits <- do.call(rsample::bootstraps, args)
    if (isTRUE(getOption("marginaleffects_parallel_inferences", default = FALSE))) {
        insight::check_if_installed("future.apply")
        pkg <- getOption("marginaleffects_parallel_packages", default = NULL)
        pkg <- unique(c("marginaleffects", pkg))
        splits$estimates <- future.apply::future_lapply(
            splits$splits,
            bootfun_term,
            future.seed = TRUE,
            future.packages = pkg
        )
    } else {
        splits$estimates <- lapply(splits$splits, bootfun_term)
    }

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

    if (all(out$term == seq_len(nrow(out)))) {
        out$term <- NULL
    }

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
