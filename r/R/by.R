harmonize_by_types <- function(estimates, by) {
    for (v in colnames(by)) {
        if (isTRUE(is.character(estimates[[v]])) && isTRUE(is.numeric(by[[v]]))) {
            by[[v]] <- as.character(by[[v]])
        } else if (isTRUE(is.numeric(estimates[[v]])) && isTRUE(is.character(by[[v]]))) {
            by[[v]] <- as.numeric(by[[v]])
        }
    }
    return(by)
}


# Resolve the `by` argument against a table of estimates: merge grouping
# columns which live only in `newdata`, join a `by` data frame of labels,
# warn about and drop rows a partial `by` data frame does not cover, and
# return the grouping column names. Shared by the display aggregation
# (get_by) and the replay recording (record_plan_aggregation) so the two can
# never disagree about what a group is.
resolve_by_rows <- function(estimates, newdata, by, verbose = TRUE, draws = NULL) {
    missing <- setdiff(setdiff(colnames(by), "by"), colnames(estimates))
    if (length(missing) > 0) {
        estimates <- merge_original_data(
            estimates,
            newdata,
            keys = c("rowid", "rowidcf"),
            payload = missing,
            unit_level_only = FALSE
        )
    }

    if (isTRUE(by)) {
        regex <- "^term$|^group$|^contrast$|^contrast_"
        bycols <- grep(regex, colnames(estimates), value = TRUE)
    } else if (isTRUE(checkmate::check_character(by))) {
        bycols <- by
    } else if (isTRUE(checkmate::check_data_frame(by))) {
        idx <- setdiff(intersect(colnames(estimates), colnames(by)), "by")
        by <- harmonize_by_types(estimates, by)
        estimates[by, by := by, on = idx]
        bycols <- "by"
    }

    if ("by" %in% colnames(estimates) && anyNA(estimates[["by"]])) {
        # User-supplied by data can intentionally cover only some rows.
        msg <- insight::format_message(
            "The `by` data.frame does not cover all combinations of response levels and/or predictors. Some estimates will not be included in the aggregation."
        )
        if (isTRUE(verbose)) warning(msg, call. = FALSE)
        tmp <- !is.na(estimates[["by"]])
        if (!is.null(draws)) {
            draws <- draws[tmp, , drop = FALSE]
        }
        estimates <- estimates[tmp, drop = FALSE]
    }

    bycols <- intersect(unique(c("term", bycols)), colnames(estimates))
    list(estimates = estimates, draws = draws, bycols = bycols)
}


get_by <- function(
    estimates,
    draws,
    newdata,
    by,
    verbose = TRUE,
    ...) {
    if (is.null(by) || isFALSE(by) || nrow(estimates) <= 1) {
        out <- estimates
        attr(out, "posterior_draws") <- draws
        return(out)
    }

    resolved <- resolve_by_rows(
        estimates,
        newdata,
        by,
        verbose = verbose,
        draws = draws
    )
    estimates <- resolved$estimates
    draws <- resolved$draws
    bycols <- resolved$bycols

    # bayesian
    if (!is.null(draws)) {
        estimates <- average_draws(
            data = estimates,
            index = bycols,
            draws = draws
        )

        # frequentist
    } else {
        if ("marginaleffects_wts_internal" %in% colnames(newdata)) {
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
