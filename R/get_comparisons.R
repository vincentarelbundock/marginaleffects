# Important: lo and hi must be made simultaneously to for seed-related issues in random effects models
get_predictions_bayesian <- function(model, type, lo, hi, ...) {
    both <- rbindlist(list(lo, hi))
    
    pred_both <- get_predict_error(
        model,
        type = type,
        newdata = both,
        ...
    )
    
    data.table::setDT(pred_both)
    pred_both[, "lo" := seq_len(.N) <= .N / 2, by = "group"]
    
    pred_lo <- pred_both[pred_both$lo, .(rowid, group, estimate), drop = FALSE]
    pred_hi <- pred_both[!pred_both$lo, .(rowid, group, estimate), drop = FALSE]
    data.table::setDF(pred_lo)
    data.table::setDF(pred_hi)
    
    draws <- attr(pred_both, "posterior_draws")
    draws_lo <- draws[pred_both$lo, , drop = FALSE]
    draws_hi <- draws[!pred_both$lo, , drop = FALSE]
    
    attr(pred_lo, "posterior_draws") <- draws_lo
    attr(pred_hi, "posterior_draws") <- draws_hi
    
    list(pred_lo = pred_lo, pred_hi = pred_hi, draws_lo = draws_lo, draws_hi = draws_hi)
}


get_predictions_frequentist <- function(model, type, lo, hi, ...) {
    pred_lo <- get_predict_error(
        model,
        type = type,
        newdata = lo,
        ...
    )
    
    pred_hi <- get_predict_error(
        model,
        type = type,
        newdata = hi,
        ...
    )
    
    list(pred_lo = pred_lo, pred_hi = pred_hi, draws_lo = NULL, draws_hi = NULL)
}


# Internal helper: process elasticity variable values
process_elasticity_variables <- function(variables, out, original, by) {
    elasticity_types <- c("eyex", "eydx", "dyex", "eyexavg", "eydxavg", "dyexavg")

    # Filter variables that need elasticity values (x parameter)
    FUN <- function(z) {
        (is.character(z$comparison) && z$comparison %in% elasticity_types) ||
            (is.function(z$comparison) && "x" %in% names(formals(z$comparison)))
    }
    elasticity_vars <- Filter(FUN, variables)
    elasticity_vars <- lapply(elasticity_vars, function(x) x$name)

    if (length(elasticity_vars) == 0) return(elasticity_vars)

    # assigning a subset of "original" to "idx1" takes time and memory
    # better to do this here for most columns and add the "v" column only
    # in the loop
    if (!is.null(original)) {
        idx1 <- c(
            "rowid",
            "rowidcf",
            "term",
            "group",
            grep("^contrast", colnames(original), value = TRUE)
        )
        idx1 <- intersect(idx1, colnames(original))
        idx1 <- original[, ..idx1]
    }

    for (v in names(elasticity_vars)) {
        idx2 <- unique(c(
            "rowid",
            "term",
            "group",
            by,
            grep("^contrast", colnames(out), value = TRUE)
        ))
        idx2 <- intersect(idx2, colnames(out))
        # discard other terms to get right length vector
        idx2 <- out[term == v, ..idx2]
        # original is NULL when cross=TRUE
        if (!is.null(original)) {
            # if not first iteration, need to remove previous "v" and "elast"
            if (v %in% colnames(idx1)) {
                idx1[, (v) := NULL]
            }
            if ("elast" %in% colnames(idx1)) {
                idx1[, elast := NULL]
            }
            idx1[, (v) := original[[v]]]
            setnames(idx1, old = v, new = "elast")
            on_cols <- intersect(colnames(idx1), colnames(idx2))
            idx2 <- unique(merge(idx2, idx1, by = on_cols, sort = FALSE)[,
                elast := elast
                ])
        }
        elasticity_vars[[v]] <- idx2$elast
    }

    return(elasticity_vars)
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
    data.table::setDT(newdata)
    
    # get_se_delta() needs perturbed coefficients model
    model <- if (is.null(model_perturbed)) mfx@model else model_perturbed
    
    # some predict() methods need data frames and will convert data.tables
    # internally, which can be very expensive if done many times. we do it once
    # here.
    data.table::setDF(lo)
    data.table::setDF(hi)
    data.table::setDF(original)
    
    # Get predictions using appropriate strategy
    if (inherits(model, c("brmsfit", "bart"))) {
        pred_result <- get_predictions_bayesian(model, type, lo, hi, ...)
    } else {
        pred_result <- get_predictions_frequentist(model, type, lo, hi, ...)
    }
    
    pred_lo <- pred_result$pred_lo
    pred_hi <- pred_result$pred_hi
    draws_lo <- pred_result$draws_lo
    draws_hi <- pred_result$draws_hi
    
    # lots of indexing later requires a data.table
    data.table::setDT(original)
    
    elasticity_types <- c("eyex", "eydx", "dyex", "eyexavg", "eydxavg", "dyexavg")
    fun <- function(x) {
        out <- checkmate::check_choice(x$comparison, choices = elasticity_types)
        isTRUE(out)
    }
    elasticity_vars_needed <- Filter(fun, variables)

    # output data.frame
    out <- pred_lo
    data.table::setDT(out)

    # Get pred_or is required for elasticity computation and it is expensive. do it once
    if (!isTRUE(deltamethod) || length(elasticity_vars_needed) > 0) {
        pred_or <- get_predict_error(
            model,
            type = type,
            newdata = original,
            ...
        )
        data.table::setDT(pred_or)
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
        out <- merge(out, newdata, by = "rowid", all.x = TRUE, sort = FALSE)
        if (isTRUE(nrow(out) == nrow(lo))) {
            contrast_cols <- data.table(lo)[,
                .SD,
                .SDcols = patterns("^contrast|marginaleffects_wts_internal")
            ]
            out <- cbind(out, contrast_cols)
            idx <- c(
                "rowid",
                grep("^contrast", colnames(out), value = TRUE),
                colnames(out)
            )
            idx <- unique(idx)
            out <- out[, ..idx]
        }
    }

    if (!"term" %in% colnames(out)) {
        out[, "term" := "cross"]
    }

    # by
    if (isTRUE(checkmate::check_data_frame(by))) {
        bycols <- "by"
        data.table::setDT(by)
        by_common_cols <- setdiff(intersect(colnames(out), colnames(by)), "by")

        if (length(by_common_cols) == 0) {
            if (all(colnames(by) %in% c("by", colnames(newdata)))) {
                nd <- c("rowid", "rowid_dedup", setdiff(colnames(by), "by"))
                nd <- intersect(nd, colnames(newdata))
                nd <- newdata[, ..nd, drop = FALSE]
                bycol <- intersect(c("rowid", "rowid_dedup"), colnames(nd))
                out <- merge(out, nd, by = bycol, sort = FALSE)
                by_common_cols <- setdiff(intersect(colnames(out), colnames(by)), "by")
            } else {
                stop_sprintf(
                    "The column in `by` must be present in `newdata`."
                )
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
    } else if (isTRUE(by)) {
        regex <- "^term$|^contrast_?|^group$"
        by <- grep(regex, colnames(out), value = TRUE)
        by <- unique(by)
    } else if (isTRUE(checkmate::check_character(by))) {
        regex <- "^term$|^contrast_?|^group$"
        by <- c(by, grep(regex, colnames(out), value = TRUE))
        by <- unique(by)
    }

    # comparison function could be different for different terms
    # sanitize_variables() ensures all functions are identical when there are cross
    fun_list <- sapply(names(variables), function(x) variables[[x]][["function"]])
    fun_list[["cross"]] <- fun_list[[1]]

    # Process elasticity variables (filtering and values)
    elasticity_vars <- process_elasticity_variables(variables, out, original, by)

    draws <- attr(pred_lo, "posterior_draws")

    # frequentist
    if (is.null(draws)) {
        draws_lo <- draws_hi <- draws_or <- NULL
    } else {
        # bayes
        draws_lo <- attr(pred_lo, "posterior_draws")
        draws_hi <- attr(pred_hi, "posterior_draws")
        draws_or <- attr(pred_or, "posterior_draws")
    }

    data.table::setDT(pred_hi)

    out[, predicted_lo := pred_lo[["estimate"]]]
    out[, predicted_hi := pred_hi[["estimate"]]]


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
    # TODO: What is the UI for this? Doesn't make sense to have different functions.
    if (isTRUE(checkmate::check_character(by))) {
        merge_cols <- intersect(colnames(newdata), c(by, colnames(out)))
        if (length(merge_cols) > 1) {
            newdata_subset <- subset(newdata, select = merge_cols)
            out <- merge(out, newdata_subset, all.x = TRUE, sort = FALSE)
            idx <- unique(c(idx, by))
        }
    }

    # we feed these columns to compare_hi_lo(), even if they are useless for categoricals
    if (!"marginaleffects_wts_internal" %in% colnames(out)) {
        out[, "marginaleffects_wts_internal" := NA]
    }

    # need a temp index for group-by operations when elasticities is a vector of length equal to full rows of `out`
    grouping_cols <- grep("^term$|^contrast|^group$", colnames(out), value = TRUE)
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

        if (isTRUE(checkmate::check_character(by, min.len = 1))) {
            by_idx <- out[, ..by]
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
        # We want to write the "estimate" column in-place because it safer
        # than group-merge; there were several bugs related to this in the past.
        # compare_hi_lo() returns 1 value and NAs when the function retunrs a
        # singleton.
        idx <- intersect(idx, colnames(out))
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
            keyby = idx
        ]
        out[, tmp_idx := NULL]

        # if comparison returns a single value, then we padded with NA. That
        # also means we don't want `rowid` otherwise we will merge and have
        # useless duplicates.
        if (anyNA(out$estimate)) {
            if ("rowid" %in% colnames(out)) out[, "rowid" := NULL]
        }
        out <- stats::na.omit(out, cols = "estimate")
    }

    # clean
    if ("rowid_dedup" %in% colnames(out)) {
        out[, "rowid_dedup" := NULL]
    }

    # averaging by groups
    # sometimes this work is already done
    # if `by` is a column name, then we have merged-in a data frame earlier
    auto_mean_fun_sub <- any(grepl("^mean\\(", unique(out$contrast)))
    if (nrow(out) > 1) {
        if (
            !auto_mean_fun_sub &&
                !(is.null(by) || isFALSE(by)) &&
                any(grepl("^contrast[_]?", colnames(out)))
        ) {
            out <- get_by(
                out,
                draws = draws,
                newdata = newdata,
                by = by,
                verbose = verbose
            )
            draws <- attr(out, "posterior_draws")
        } else {
            bycols <- c(by, "group", "term", "^contrast[_]?")
            bycols <- paste(bycols, collapse = "|")
            bycols <- grep(bycols, colnames(out), value = TRUE)
        }
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
