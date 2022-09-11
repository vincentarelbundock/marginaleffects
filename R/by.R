get_by <- function(estimates, draws, newdata, by, column) {
    if (is.null(by)) {
        out <- estimates
        attr(out, "posterior_draws") <- draws
        return(out)
    }

    setnames(estimates, old = column, new = "estimate")

    bycols <- sort(setdiff(
        unique(c(colnames(estimates), colnames(newdata))),
        c("rowid", "rowidcf", "estimate", "predicted", "predicted_lo", "predicted_hi", "dydx", "comparison")))
    bycols <- paste(bycols, collapse = ", ")
    flagA1 <- checkmate::check_character(by)
    flagA2 <- checkmate::check_true(all(by %in% c(colnames(estimates), colnames(newdata))))
    flagB1 <- checkmate::check_data_frame(by)
    flagB2 <- checkmate::check_true("by" %in% colnames(by))

    missing <- setdiff(setdiff(colnames(by), "by"), colnames(estimates))
    if (length(missing) > 0) {
        idx <- intersect(c("rowid", "rowidcf", missing), colnames(newdata))
        estimates <- merge(estimates, newdata[, idx], sort = FALSE, all.x = TRUE)
    }

    if (!(isTRUE(flagA1) && isTRUE(flagA2)) && !(isTRUE(flagB1) && isTRUE(flagB2))) {
        msg <- c(
            "The `by` argument must be either:", "",
            sprintf("1. Character vector in which each element is part of: %s", bycols),
            "",
            sprintf("2. A data frame with a `by` column of labels, and in which all other columns are elements of: %s", bycols),
            "",
            "It can sometimes be useful to supply a data frame explicitly to the `newdata` argument in order to be able to group by all available columns."
        )
        stop(insight::format_message(msg), call. = FALSE)
    }

    # `by` data.frame
    if (isTRUE(checkmate::check_data_frame(by))) {
        idx <- setdiff(intersect(colnames(estimates), colnames(by)), "by")
        for (v in colnames(by)) {
            if (isTRUE(is.character(estimates[[v]])) && isTRUE(is.numeric(by[[v]]))) {
                by[[v]] <- as.character(by[[v]])
            } else if (isTRUE(is.numeric(estimates[[v]])) && isTRUE(is.character(by[[v]]))) {
                by[[v]] <- as.numeric(by[[v]])
            }
        }
        estimates[by, by := by, on = idx]
        bycols <- "by"

        # `by` vector
    } else {
        bycols <- by
    }

    bycols <- intersect(unique(c("term", bycols)), colnames(estimates))

    # bayesian
    if (!is.null(draws)) {
        estimates <- average_draws(
            data = estimates,
            index = bycols,
            draws = draws,
            column = "estimate")

        # frequentist
    } else {
        if ("marginaleffects_wts_internal" %in% colnames(newdata)) {
            estimates <- estimates[,
                .(estimate = stats::weighted.mean(
                    estimate,
                    marginaleffects_wts_internal,
                    na.rm = TRUE)),
                by = bycols]
        } else {
            estimates <- estimates[,
                .(estimate = mean(estimate)),
                by = bycols]
        }
    }

    setnames(estimates, old = "estimate", new = column)
    return(estimates)
}