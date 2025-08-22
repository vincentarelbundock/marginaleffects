# Important: lo and hi must be made simultaneously for seed-related issues in random effects models
get_comparison_predictions <- function(model, type, lo, hi, ...) {
    both <- data.table::rbindlist(list(lo, hi), idcol = "src")  # 1=lo, 2=hi
    pred <- get_predict_error(model, type = type, newdata = both, ...)
    
    # Handle case where get_predict_error changes row count
    if (nrow(pred) == nrow(both)) {
        pred[, src := both$src]
    } else {
        # Fallback: use half-and-half split (original approach)
        pred[, src := rep(1:2, each = .N/2)]
    }
    
    draws <- attr(pred, "posterior_draws")
    
    # Select columns that exist (group may not always be present)
    cols <- intersect(c("rowid", "group", "estimate"), names(pred))
    pred_lo <- pred[src == 1L, ..cols]
    pred_hi <- pred[src == 2L, ..cols]
    
    draws_lo <- if (!is.null(draws)) draws[pred$src == 1L, , drop = FALSE] else NULL
    draws_hi <- if (!is.null(draws)) draws[pred$src == 2L, , drop = FALSE] else NULL
    
    attr(pred_lo, "posterior_draws") <- draws_lo
    attr(pred_hi, "posterior_draws") <- draws_hi
    
    list(pred_lo = pred_lo, pred_hi = pred_hi, draws_lo = draws_lo, draws_hi = draws_hi)
}




# Internal helper: process elasticity variable values
process_elasticity_variables <- function(variables, out, original, by, contrast_cols) {
    types <- c("eyex","eydx","dyex","eyexavg","eydxavg","dyexavg")
    needs <- vapply(variables, function(z)
        is.character(z$comparison) && z$comparison %in% types ||
        (is.function(z$comparison) && "x" %in% names(formals(z$comparison))),
        logical(1))
    nms <- names(variables)[needs]
    if (!length(nms)) return(list())

    elast <- vector("list", length(nms)); names(elast) <- nms

    if (!is.null(original)) {
        base_cols <- intersect(c("rowid","rowidcf","term","group", contrast_cols),
                               names(original))
        idx1 <- original[, ..base_cols, drop = FALSE]
    }

    # columns we need from `out` once
    out_keep <- unique(c("rowid","term","group", by, contrast_cols))
    out_keep <- intersect(out_keep, names(out))

    for (v in nms) {
        idx2 <- out[term == v, ..out_keep]
        if (!is.null(original)) {
            idx1[, elast := original[[v]]]
            on_cols <- intersect(names(idx1), names(idx2))
            idx2 <- unique(merge(idx2, idx1, by = on_cols, sort = FALSE))
        }
        elast[[v]] <- if (!is.null(original)) idx2$elast else NULL
    }
    elast
}


# safe version of comparison
# unknown arguments
# singleton vs vector
# different terms use different functions
compare_hi_lo <- function(hi, lo, y, n, term, cross, wts, tmp_idx, newdata, variables, fun_list, elasticity_vars) {
    tn <- term[1]
    eps <- variables[[tn]]$eps

    # when cross=TRUE, sanitize_comparison enforces a single function
    if (isTRUE(cross)) {
        fun <- fun_list[[1]]
    } else {
        fun <- fun_list[[tn]]
    }

    args <- list(
        "hi" = hi,
        "lo" = lo,
        "y" = y,
        "eps" = eps,
        "w" = wts,
        "newdata" = newdata
    )

    # sometimes x is exactly the same length, but not always
    args[["x"]] <- elasticity_vars[[tn]][tmp_idx]

    args <- args[names(args) %in% names(formals(fun))]
    con <- try(do.call("fun", args), silent = TRUE)

    # fmt: skip
    flag1 <- !isTRUE(checkmate::check_numeric(con, len = n))
    flag2 <- !isTRUE(checkmate::check_numeric(con, len = 1))
    if (flag1 && flag2) {
        msg <- sprintf(
            "The function supplied to the `comparison` argument must accept two numeric vectors of predicted probabilities of length %s, and return a single numeric value or a numeric vector of length %s, with no missing value.",
            n, n)
        stop_sprintf(msg)
    }

    # If function returns single value, pad with NAs  
    if (length(con) == 1) {
        con <- c(con, rep(NA_real_, length(hi) - 1))
    }
    return(con)
}

# Clean up temporary columns in one pass
clean_temp_columns <- function(dt) {
    cols <- intersect(c("rowid_dedup","tmp_idx"), names(dt))
    if (length(cols)) dt[, (cols) := NULL]
    dt
}


get_comparisons <- function(
    mfx,
    type,
    variables,
    original,
    lo,
    hi,
    model_perturbed = NULL,
    by = NULL,
    byfun = NULL,
    hypothesis = NULL,
    cross = FALSE,
    verbose = TRUE,
    deltamethod = FALSE,
    ...
) {
    newdata <- mfx@newdata
    
    # get_se_delta() needs perturbed coefficients model
    model <- if (is.null(model_perturbed)) mfx@model else model_perturbed
    
    # Get predictions (unified function handles both Bayesian and frequentist models)
    pred_result <- get_comparison_predictions(model, type, lo, hi, ...)
    
    pred_lo <- pred_result$pred_lo
    pred_hi <- pred_result$pred_hi
    draws_lo <- pred_result$draws_lo
    draws_hi <- pred_result$draws_hi
    
    # original stays as data.frame for predict() methods, convert to DT only when needed for indexing
    
    elasticity_types <- c("eyex", "eydx", "dyex", "eyexavg", "eydxavg", "dyexavg")
    fun <- function(x) {
        out <- checkmate::check_choice(x$comparison, choices = elasticity_types)
        isTRUE(out)
    }
    elasticity_vars_needed <- Filter(fun, variables)

    # output data.frame
    out <- pred_lo

    # Precompute regex matches to avoid repeated pattern matching
    contrast_cols <- grep("^contrast", colnames(out), value = TRUE)
    base_grouping_regex <- "^term$|^group$|^type$|^comparison_idx$"

    # Check if any variables actually need x values for elasticity computation
    has_elasticity_vars <- length(elasticity_vars_needed) > 0
    # Get pred_or only when needed (expensive operation)
    need_pred_or <- !isTRUE(deltamethod) || has_elasticity_vars
    if (need_pred_or) {
        pred_or <- get_predict_error(
            model,
            type = type,
            newdata = original,
            ...
        )
        out[, predicted := pred_or[["estimate"]]]
    } else {
        out[, predicted := NA_real_]
    }

    # univariate outcome:
    # original is the "composite" data that we constructed by binding terms and
    # compute predictions. It includes a term column, which we need to
    # replicate for each group.
    out[, "marginaleffects_wts_internal" := NA_real_] # default (probably almost always overwritten)
    mult <- nrow(out) / nrow(original)
    regex <- "^term$|rowid_dedup|^group$|^contrast|^marginaleffects_wts_internal$"
    if (isTRUE(mult == 1)) {
        for (v in grep(regex, colnames(original), value = TRUE)) {
            out[, (v) := original[[v]]]
        }

        # group or multivariate outcomes
    } else if (isTRUE(mult > 1)) {
        for (v in grep(regex, colnames(original), value = TRUE)) {
            out[, (v) := rep(original[[v]], times = mult)]
        }

        # cross-contrasts or weird cases
    } else {
        # Use data.table join for better performance (newdata already DT from boundary)
        data.table::setkeyv(out, "rowid")
        data.table::setkeyv(newdata, "rowid") 
        out <- newdata[out, on = "rowid"]
        if (isTRUE(nrow(out) == nrow(lo))) {
            contrast_cols <- data.table(lo)[,
                .SD,
                .SDcols = patterns("^contrast|marginaleffects_wts_internal")
            ]
            # Use data.table column binding to avoid conversion to data.frame
            contrast_col_names <- names(contrast_cols)
            for (col in contrast_col_names) {
                out[[col]] <- contrast_cols[[col]]
            }
            idx <- c(
                "rowid",
                contrast_cols,
                colnames(out)
            )
            idx <- unique(idx)
            # Remove unwanted columns in-place instead of copying
            cols_to_remove <- setdiff(names(out), idx)
            if (length(cols_to_remove)) out[, (cols_to_remove) := NULL]
        }
    }

    if (!"term" %in% colnames(out)) {
        out[, "term" := "cross"]
    }

    # Centralize by construction - normalize by into character vector once
    by_cols <- character()
    by_is_true <- isTRUE(by)
    by_is_df <- isTRUE(checkmate::check_data_frame(by))
    by_is_char <- isTRUE(checkmate::check_character(by))
    
    regex <- "^term$|^contrast_?|^group$"
    if (by_is_true) {
        by_cols <- grep(regex, colnames(out), value = TRUE)
    } else if (by_is_char) {
        by_cols <- unique(c(by, grep(regex, colnames(out), value = TRUE)))
    } else if (by_is_df) {
        data.table::setDT(by)
        by_cols <- "by"
        by_common_cols <- setdiff(
            intersect(colnames(out), colnames(by)), 
            by_cols)

        if (length(by_common_cols) == 0) {
            if (all(colnames(by) %in% c("by", colnames(newdata)))) {
                nd <- c("rowid", "rowid_dedup", setdiff(colnames(by), "by"))
                nd <- intersect(nd, colnames(newdata))
                nd <- newdata[, ..nd, drop = FALSE]
                bycol <- intersect(c("rowid", "rowid_dedup"), colnames(nd))
                data.table::setkeyv(out, bycol)
                data.table::setkeyv(nd, bycol)
                out <- out[nd, on = bycol, nomatch = 0L]
                by_common_cols <- setdiff(intersect(colnames(out), colnames(by)), "by")
            } else {
                stop_sprintf("The column in `by` must be present in `newdata`.")
            }
        }

        # harmonize column types
        for (v in colnames(by)) {
            if (isTRUE(is.character(out[[v]])) && isTRUE(is.numeric(by[[v]]))) {
                by[[v]] <- as.character(by[[v]])
            } else if (isTRUE(is.numeric(out[[v]])) && isTRUE(is.character(by[[v]]))) {
                by[[v]] <- as.numeric(by[[v]])
            }
        }
        out[by, by := by, on = by_common_cols]
        by <- "by"
    }

    # Build function list once with guaranteed cross entry
    fun_list <- lapply(variables, `[[`, "function")
    if (!length(fun_list)) stop("No comparison function found.")
    fun_list[["cross"]] <- fun_list[[1L]]

    # Process elasticity variables (filtering and values)
    elasticity_vars <- process_elasticity_variables(variables, out, original, by_cols, contrast_cols)

    draws <- attr(pred_lo, "posterior_draws")
    draws_lo <- attr(pred_lo, "posterior_draws")
    draws_hi <- attr(pred_hi, "posterior_draws")
    draws_or <- attr(pred_or, "posterior_draws")

    # Assign all prediction estimates at once
    out[, `:=`(
        predicted_lo = pred_lo[["estimate"]],
        predicted_hi = pred_hi[["estimate"]]
    )]

    idx <- grep(
        "^contrast|^group$|^term$|^type$|^comparison_idx$",
        colnames(out),
        value = TRUE
    )

    # when `by` is a character vector, we sometimes modify the comparison
    # function on the fly to use the `avg` version.  this is important and
    # convenient because some of the statistics are non-collapsible, so we can't
    # average them at the very end.  when `by` is a data frame, we do this only
    # at the very end.
    if (by_is_char) {
        merge_cols <- intersect(colnames(newdata), c(by_cols, colnames(out)))
        if (length(merge_cols) > 1) {
            newdata_subset <- newdata[, ..merge_cols]
            data.table::setkeyv(out, merge_cols)
            data.table::setkeyv(newdata_subset, merge_cols)
            out <- newdata_subset[out, on = merge_cols]
            idx <- unique(c(idx, by_cols))
        }
    }

    # we feed these columns to compare_hi_lo(), even if they are useless for categoricals
    if (!"marginaleffects_wts_internal" %in% colnames(out)) {
        out[, "marginaleffects_wts_internal" := NA]
    }

    # need a temp index for group-by operations when elasticities is a 
    # vector of length equal to full rows of `out`
    grouping_cols <- c(
        grep(base_grouping_regex, colnames(out), value = TRUE),
        contrast_cols
    )
    if (length(grouping_cols) > 0) {
        out[, tmp_idx := seq_len(.N), by = grouping_cols]
    } else {
        out[, tmp_idx := seq_len(.N)]
    }

    # bayesian
    if (!is.null(draws)) {
        # drop missing otherwise get_averages() fails when trying to take a
        # simple mean
        idx_na <- !is.na(out$predicted_lo)
        out <- stats::na.omit(out, cols = "predicted_lo")

        # TODO: performance is probably terrrrrible here, but splitting is
        # tricky because grouping rows are not always contiguous, and the order
        # of rows is **extremely** important because draws don't have the
        # indices that would allow us to align them back with `out`
        draws <- draws[idx_na, , drop = FALSE]

        if (by_is_char && length(by_cols) > 0) {
            by_idx <- out[, ..by_cols]
            by_idx <- do.call(paste, c(by_idx, sep = "|"))
        } else {
            by_idx <- out$term
        }

        # loop over columns (draws) and term names because different terms could use different functions
        for (tn in unique(by_idx)) {
            for (i in seq_len(ncol(draws))) {
                idx <- by_idx == tn
                draws[idx, i] <- compare_hi_lo(
                    hi = draws_hi[idx, i],
                    lo = draws_lo[idx, i],
                    y = draws_or[idx, i],
                    n = sum(idx),
                    term = out$term[idx],
                    cross = cross,
                    wts = out$marginaleffects_wts_internal[idx],
                    tmp_idx = out$tmp_idx[idx],
                    newdata = newdata,
                    variables = variables,
                    fun_list = fun_list,
                    elasticity_vars = elasticity_vars
                )
            }
        }

        # function returns unique value
        idx <- !is.na(draws[, 1])
        draws <- draws[idx, , drop = FALSE]

        # if comparison returns a single value, then we padded with NA. That
        # also means we don't want `rowid` otherwise we will merge and have
        # useless duplicates.
        if (!all(idx)) {
            if ("rowid" %in% colnames(out)) out[, "rowid" := NULL]
            out <- out[idx, , drop = FALSE]
        }

        FUN_CENTER <- getOption(
            "marginaleffects_posterior_center",
            default = stats::median
        )
        out[, "estimate" := apply(draws, 1, FUN_CENTER)]

        # frequentist
    } else {
        out <- stats::na.omit(out, cols = "predicted_lo")
        # We want to write the "estimate" column in-place because it is safer
        # than group-merge; there were several bugs related to this in the past.
        # compare_hi_lo() returns 1 value and NAs when the function returns a
        # singleton.
        grouping_cols <- intersect(idx, colnames(out))
        out[,
            "estimate" := compare_hi_lo(
                hi = predicted_hi,
                lo = predicted_lo,
                y = predicted,
                n = .N,
                term = term,
                cross = cross,
                wts = marginaleffects_wts_internal,
                tmp_idx = tmp_idx,
                newdata = newdata,
                variables = variables,
                fun_list = fun_list,
                elasticity_vars = elasticity_vars
            ),
            by = grouping_cols
        ]

        # if comparison returns a single value, then we padded with NA. That
        # also means we don't want `rowid` otherwise we will merge and have
        # useless duplicates.
        if (anyNA(out$estimate)) {
            if ("rowid" %in% colnames(out)) out[, "rowid" := NULL]
        }
        out <- stats::na.omit(out, cols = "estimate")
    }

    # clean up temporary columns
    out <- clean_temp_columns(out)

    # averaging by groups
    # sometimes this work is already done
    # if `by` is a column name, then we have merged-in a data frame earlier
    flag1 <- !any(grepl("^mean\\(", unique(out$contrast)))
    flag2 <- !(is.null(by) || isFALSE(by))
    flag3 <- any(grepl("^contrast[_]?", colnames(out)))
    if (flag1 && flag2 && flag3) {
        out <- get_by(
            out,
            draws = draws,
            newdata = newdata,
            by = by,
            verbose = verbose
        )
        draws <- attr(out, "posterior_draws")
    }

    # before get_hypothesis
    attr(out, "posterior_draws") <- draws

    # hypothesis tests using the delta method
    out <- get_hypothesis(
        out,
        hypothesis,
        by = by,
        newdata = original,
        draws = draws
    )

    return(out)
}
