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
# return the grouping column names. Used by record_plan_aggregation(), which
# records the aggregation stage once for point estimates, posterior draws,
# and every replay.
resolve_by_rows <- function(estimates, newdata, by, verbose = TRUE) {
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
        estimates <- estimates[tmp, drop = FALSE]
    }

    bycols <- intersect(unique(c("term", bycols)), colnames(estimates))
    list(estimates = estimates, bycols = bycols)
}
