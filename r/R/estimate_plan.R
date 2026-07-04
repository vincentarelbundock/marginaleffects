estimate_plan_predict_dots <- function(base, extra = list()) {
    out <- utils::modifyList(base %||% list(), extra %||% list())
    drop <- c(
        "mfx", "model", "model_perturbed", "hypothesis", "hi", "lo",
        "original", "by", "variables", "cross", "estimates",
        "vcov", "FUN", "index", "numderiv", "J", "newdata", "type",
        "comparison", "calling_function"
    )
    out[setdiff(names(out), drop)]
}

estimate_plan_record_agg <- function(
    estimates,
    newdata,
    by,
    verbose = TRUE,
    ...) {
    if (is.null(by) || isFALSE(by) || nrow(estimates) <= 1) {
        return(list(out = estimates, agg = NULL))
    }

    estimates <- data.table::copy(estimates)
    plan_id <- ".marginaleffects_plan_est_id"
    while (plan_id %in% colnames(estimates)) {
        plan_id <- paste0(".", plan_id)
    }
    estimates[, (plan_id) := seq_len(.N)]

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
        by <- harmonize_by_types(estimates, by)
        estimates[by, by := by, on = idx]
        bycols <- "by"
    }

    if ("by" %in% colnames(estimates) && anyNA(estimates[["by"]])) {
        msg <- insight::format_message(
            "The `by` data.frame does not cover all combinations of response levels and/or predictors. Some estimates will not be included in the aggregation."
        )
        if (isTRUE(verbose)) warning(msg, call. = FALSE)
        estimates <- estimates[!is.na(by), drop = FALSE]
    }

    bycols <- intersect(unique(c("term", bycols)), colnames(estimates))
    weighted <- "marginaleffects_wts_internal" %in% colnames(newdata)

    if (isTRUE(weighted)) {
        out <- estimates[,
            .(estimate = stats::weighted.mean(
                estimate,
                marginaleffects_wts_internal,
                na.rm = TRUE
            )),
            keyby = bycols
        ]
    } else {
        out <- estimates[, .(estimate = mean(estimate, na.rm = TRUE)), keyby = bycols]
    }

    groups_dt <- estimates[,
        .(
            idx = list(get(plan_id)),
            w = list(if (isTRUE(weighted)) marginaleffects_wts_internal else NULL)
        ),
        keyby = bycols
    ]

    groups <- lapply(seq_len(nrow(groups_dt)), function(i) {
        list(idx = groups_dt[["idx"]][[i]], w = groups_dt[["w"]][[i]])
    })

    agg <- list(groups = groups)
    return(list(out = out, agg = agg))
}

estimate_plan_apply_agg <- function(agg, est) {
    out <- numeric(length(agg$groups))
    for (j in seq_along(agg$groups)) {
        gr <- agg$groups[[j]]
        e <- est[gr$idx]
        out[j] <- if (!is.null(gr$w)) {
            stats::weighted.mean(e, gr$w, na.rm = TRUE)
        } else {
            mean(e, na.rm = TRUE)
        }
    }
    out
}
