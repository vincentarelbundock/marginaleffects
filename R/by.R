get_by <- function(
    estimates,
    draws,
    newdata,
    by,
    byfun = NULL,
    verbose = TRUE,
    ...) {
    if (is.null(by) || isFALSE(by)) {
        out <- estimates
        attr(out, "posterior_draws") <- draws
        return(out)
    }

    missing <- setdiff(setdiff(colnames(by), "by"), colnames(estimates))
    if (length(missing) > 0) {
        idx <- intersect(c("rowid", "rowidcf", missing), colnames(newdata))
        estimates <- merge(estimates, newdata[, idx], sort = FALSE, all.x = TRUE)
    }

    if (isTRUE(by)) {
        regex <- "^term$|^group$|^contrast$|^contrast_"
        bycols <- grep(regex, colnames(estimates), value = TRUE)
    } else if (isTRUE(checkmate::check_character(by))) {
        bycols <- by
    } else if (isTRUE(checkmate::check_data_frame(by))) {
        idx <- setdiff(intersect(colnames(estimates), colnames(by)), "by")
        # harmonize column types
        for (v in colnames(by)) {
            if (isTRUE(is.character(estimates[[v]])) && isTRUE(is.numeric(by[[v]]))) {
                by[[v]] <- as.character(by[[v]])
            } else if (isTRUE(is.numeric(estimates[[v]])) && isTRUE(is.character(by[[v]]))) {
                by[[v]] <- as.numeric(by[[v]])
            }
        }
        estimates[by, by := by, on = idx]
        bycols <- "by"
    }

    if ("by" %in% colnames(estimates) && anyNA(estimates[["by"]])) {
        msg <- insight::format_message(
            "The `by` data.frame does not cover all combinations of response levels and/or predictors. Some estimates will not be included in the aggregation."
        )
        if (isTRUE(verbose)) warning(msg, call. = FALSE)
        tmp <- !is.na(estimates[["by"]])
        draws <- draws[tmp, drop = FALSE]
        estimates <- estimates[tmp, drop = FALSE]
    }

    bycols <- intersect(unique(c("term", bycols)), colnames(estimates))

    # bayesian
    if (!is.null(draws)) {
        estimates <- average_draws(
            data = estimates,
            index = bycols,
            draws = draws,
            byfun = byfun
        )

        # frequentist
    } else {
        if (!is.null(byfun)) {
            estimates <- estimates[,
                .(estimate = byfun(estimate)),
                keyby = bycols
            ]
        } else if ("marginaleffects_wts_internal" %in% colnames(newdata)) {
            estimates <- estimates[,
                .(
                    estimate = stats::weighted.mean(
                        estimate,
                        marginaleffects_wts_internal,
                        na.rm = TRUE
                    )
                ),
                keyby = bycols
            ]
        } else {
            estimates <- estimates[,
                .(estimate = mean(estimate, na.rm = TRUE)),
                keyby = bycols
            ]
        }
    }

    return(estimates)
}
