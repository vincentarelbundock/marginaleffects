get_comparisons <- function(
    mfx,
    type,
    variables,
    original,
    lo,
    hi,
    pred_or,
    model_perturbed = NULL,
    by = NULL,
    byfun = NULL,
    hypothesis = NULL,
    cross = FALSE,
    verbose = TRUE,
    ...) {
    newdata <- mfx@newdata
    data.table::setDT(newdata)

    # pred_or -> out backbone
    out <- as.data.table(pred_or)
    out[, predicted := estimate]
    out[, estimate := NULL]

    # get_se_delta() needs perturbed coefficients model
    model <- if (is.null(model_perturbed)) mfx@model else model_perturbed

    settings_init()

    predictions <- predictions_hi_lo(model, lo, hi, type, ...)
    list2env(predictions, environment())

    # predict() takes up 2/3 of the wall time. This call is only useful when we
    # compute elasticities, or for the main estimate, not for standard errors,
    # so we probably save 1/3 of that 2/3.
    elasticities <- c(
        # "dydx", # useless and expensive
        "eyex",
        "eydx",
        "dyex",
        # "dydxavg", # useless and expensive
        "eyexavg",
        "eydxavg",
        "dyexavg"
    )
    fun <- function(x) {
        out <- checkmate::check_choice(x$comparison, choices = elasticities)
        isTRUE(out)
    }
    tmp <- Filter(fun, variables)
    if (length(tmp) > 0) {
        new <- get_predict_error(
            model,
            type = type,
            newdata = original,
            ...
        )
        # no idea why we need to reassign because they should be the same
        pred_or <- new
    }

    # lots of indexing later requires a data.table
    data.table::setDT(original)

    # univariate outcome:
    # original is the "composite" data that we constructed by binding terms and
    # compute predictions. It includes a term column, which we need to
    # replicate for each group.
    cols <- setdiff(colnames(original), colnames(out))
    out <- cbind(out, original[, ..cols])

    if (isTRUE(cross)) {
        out <- merge(out, newdata, by = "rowid", all.x = TRUE, sort = FALSE)
        if (isTRUE(nrow(out) == nrow(lo))) {
            tmp <- data.table(lo)[,
                .SD,
                .SDcols = patterns("^contrast|marginaleffects_wts_internal")
            ]
            out <- cbind(out, tmp)
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
        data.table::setDT(by)
        tmp <- setdiff(intersect(colnames(out), colnames(by)), "by")
        # harmonize column types
        for (v in colnames(by)) {
            if (isTRUE(is.character(out[[v]])) && isTRUE(is.numeric(by[[v]]))) {
                by[[v]] <- as.character(by[[v]])
            } else if (isTRUE(is.numeric(out[[v]])) && isTRUE(is.character(by[[v]]))) {
                by[[v]] <- as.numeric(by[[v]])
            }
        }
        out[by, by := by, on = tmp]
        # only `by` because we give complete flexibility for user to aggregate across terms, groups, contrasts, etc.
        # that requires more work when building the `by` data frame, but it's more flexible
        by <- "by"
    } else if (isTRUE(by)) {
        regex <- "^term$|^contrast_?|^group$"
        by <- unique(grep(regex, colnames(out), value = TRUE))
    } else if (isTRUE(checkmate::check_character(by))) {
        regex <- "^term$|^contrast_?|^group$"
        by <- unique(c(by, grep(regex, colnames(out), value = TRUE)))
    }

    # comparison function could be different for different terms
    # sanitize_variables() ensures all functions are identical when there are cross
    fun_list <- sapply(names(variables), function(x) variables[[x]][["function"]])
    fun_list[["cross"]] <- fun_list[[1]]

    # elasticity requires the original (properly aligned) predictor values
    # this will discard factor variables which are duplicated, so in principle
    # it should be the "correct" size
    # also need `x` when `x` is in the signature of the `comparison` custom function

    FUN <- function(z) {
        (is.character(z$comparison) && z$comparison %in% elasticities) ||
            (is.function(z$comparison) && "x" %in% names(formals(z$comparison)))
    }
    elasticities <- Filter(FUN, variables)
    elasticities <- lapply(elasticities, function(x) x$name)

    if (length(elasticities) > 0) {
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

        for (v in names(elasticities)) {
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
                idx2 <- unique(merge(idx2, idx1, by = on_cols, sort = FALSE)[
                    ,
                    elast := elast
                ])
            }
            elasticities[[v]] <- idx2$elast
        }
    }

    draws <- attr(pred_lo, "posterior_draws")
    draws_lo <- attr(pred_lo, "posterior_draws")
    draws_hi <- attr(pred_hi, "posterior_draws")
    draws_or <- attr(pred_or, "posterior_draws")

    out[, predicted_lo := pred_lo[["estimate"]]]
    out[, predicted_hi := pred_hi[["estimate"]]]

    if (!is.null(pred_or)) {
        data.table::setDT(pred_or)
        out[, predicted := pred_or[["estimate"]]]
    } else {
        out[, predicted := NA_real_]
    }

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
        tmp <- intersect(colnames(newdata), c(by, colnames(out)))
        if (length(tmp) > 1) {
            tmp <- subset(newdata, select = tmp)
            out <- merge(out, tmp, all.x = TRUE, sort = FALSE)
            idx <- unique(c(idx, by))
        }
    }

    # we feed these columns to safefun(), even if they are useless for categoricals
    if (!"marginaleffects_wts_internal" %in% colnames(out)) {
        out[, "marginaleffects_wts_internal" := NA]
    }


    # need a temp index for group-by operations when elasticities is a vector of length equal to full rows of `out`
    tmp <- grep("^term$|^contrast|^group$", colnames(out), value = TRUE)
    if (length(tmp) > 0) {
        out[, tmp_idx := seq_len(.N), by = tmp]
    } else {
        out[, tmp_idx := seq_len(.N)]
    }

    if (!is.null(draws)) {
        result <- compare_hi_lo_bayesian(
            out = out,
            draws = draws,
            draws_hi = draws_hi,
            draws_lo = draws_lo,
            draws_or = draws_or,
            by = by,
            cross = cross,
            variables = variables,
            fun_list = fun_list,
            elasticities = elasticities,
            newdata = newdata
        )
        out <- result$out
        draws <- result$draws
    } else {
        out <- compare_hi_lo_frequentist(
            out = out,
            idx = idx,
            cross = cross,
            variables = variables,
            fun_list = fun_list,
            elasticities = elasticities,
            newdata = newdata
        )
    }

    # clean
    if ("rowid_dedup" %in% colnames(out)) {
        out[, "rowid_dedup" := NULL]
    }

    # averaging by groups
    # sometimes this work is already done
    # if `by` is a column name, then we have merged-in a data frame earlier
    auto_mean_fun_sub <- any(grepl("^mean\\(", unique(out$contrast)))
    if (!auto_mean_fun_sub && any(grepl("^contrast[_]?", colnames(out)))) {
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

    # reset settings
    settings_rm("marginaleffects_safefun_return1")

    return(out)
}


predictions_hi_lo <- function(model, lo, hi, type, ...) {
    # brms models need to be combined to use a single seed when sample_new_levels="gaussian"
    if (inherits(model, c("brmsfit", "bart"))) {
        if (!"rowid" %in% colnames(lo)) {
            lo$rowid <- hi$rowid <- seq_len(nrow(lo))
        }

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
    } else {
        pred_lo <- get_predict_error(
            model,
            type = type,
            newdata = lo,
            ...
        )

        pred_hi_result <- myTryCatch(get_predict(
            model,
            type = type,
            newdata = hi,
            ...
        ))

        # otherwise we keep the full error object instead of extracting the value
        if (inherits(pred_hi_result$value, "data.frame")) {
            pred_hi <- pred_hi_result$value
        } else {
            pred_hi <- pred_hi_result$error
        }
    }

    return(list(pred_lo = pred_lo, pred_hi = pred_hi))
}


compare_hi_lo_bayesian <- function(out, draws, draws_hi, draws_lo, draws_or, by, cross, variables, fun_list, elasticities, newdata) {
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
                elasticities = elasticities
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
        if (settings_equal("marginaleffects_safefun_return1", TRUE)) {
            if ("rowid" %in% colnames(out)) out[, "rowid" := NULL]
        }
        out <- out[idx, , drop = FALSE]
    }

    FUN_CENTER <- getOption(
        "marginaleffects_posterior_center",
        default = stats::median
    )
    out[, "estimate" := apply(draws, 1, FUN_CENTER)]

    return(list(out = out, draws = draws))
}


compare_hi_lo_frequentist <- function(out, idx, cross, variables, fun_list, elasticities, newdata) {
    out <- stats::na.omit(out, cols = "predicted_lo")
    # We want to write the "estimate" column in-place because it safer
    # than group-merge; there were several bugs related to this in the past.
    # safefun() returns 1 value and NAs when the function retunrs a
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
            elasticities = elasticities
        ),
        keyby = idx
    ]
    out[, tmp_idx := NULL]

    # if comparison returns a single value, then we padded with NA. That
    # also means we don't want `rowid` otherwise we will merge and have
    # useless duplicates.
    if (anyNA(out$estimate)) {
        if (settings_equal("marginaleffects_safefun_return1", TRUE)) {
            if ("rowid" %in% colnames(out)) out[, "rowid" := NULL]
        }
    }
    out <- stats::na.omit(out, cols = "estimate")

    return(out)
}


compare_hi_lo <- function(hi, lo, y, n, term, cross, wts, tmp_idx, newdata, variables, fun_list, elasticities) {
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
    args[["x"]] <- elasticities[[tn]][tmp_idx]

    args <- args[names(args) %in% names(formals(fun))]
    con <- try(do.call("fun", args), silent = TRUE)

    if (
        !isTRUE(checkmate::check_numeric(con, len = n)) &&
            !isTRUE(checkmate::check_numeric(con, len = 1))
    ) {
        msg <- sprintf(
            "The function supplied to the `comparison` argument must accept two numeric vectors of predicted probabilities of length %s, and return a single numeric value or a numeric vector of length %s, with no missing value.",
            n,
            n
        ) # nolint
        stop_sprintf(msg)
    }
    if (length(con) == 1) {
        con <- c(con, rep(NA_real_, length(hi) - 1))
        settings_set("marginaleffects_safefun_return1", TRUE)
    }
    return(con)
}
