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

        pred_both[, "lo" := seq_len(.N) <= .N / 2, by = "group"]

        pred_lo <- pred_both[pred_both$lo, .(rowid, group, estimate), drop = FALSE]
        pred_hi <- pred_both[!pred_both$lo, .(rowid, group, estimate), drop = FALSE]

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


comparison_group_indices <- function(by_idx) {
    by_idx_unique <- unique(by_idx)
    out <- vector("list", length(by_idx_unique))
    for (i in seq_along(by_idx_unique)) {
        out[[i]] <- which(by_idx == by_idx_unique[[i]])
    }
    names(out) <- as.character(by_idx_unique)
    return(out)
}


compare_hi_lo_bayesian <- function(out, draws, draws_hi, draws_lo, draws_or, by, cross, variables, fun_list, elasticities, newdata) {
    # drop missing otherwise get_averages() fails when trying to take a
    # simple mean
    idx_na <- !is.na(out$predicted_lo)

    # Build a single index and reuse it so `out` and the posterior draws remain aligned
    # while removing rows with missing `predicted_lo`.
    if (!all(idx_na)) {
        out <- out[idx_na]
        draws <- draws[idx_na, , drop = FALSE]
    }

    if (isTRUE(checkmate::check_character(by, min.len = 1))) {
        by_idx <- out[, ..by]
        by_idx <- do.call(paste, c(by_idx, sep = "|"))
    } else {
        by_idx <- out$term
    }

    # loop over columns (draws) and term names because different terms could use different functions
    group_indices <- comparison_group_indices(by_idx)
    scalar_result <- compare_hi_lo_bayesian_scalar(
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
        newdata = newdata,
        group_indices = group_indices
    )
    if (!is.null(scalar_result)) {
        return(scalar_result)
    }

    for (idx in group_indices) {
        n <- length(idx)
        term <- out$term[idx]
        wts <- out$marginaleffects_wts_internal[idx]
        tmp_idx <- out$tmp_idx[idx]
        for (i in seq_len(ncol(draws))) {
            draws[idx, i] <- compare_hi_lo(
                hi = draws_hi[idx, i],
                lo = draws_lo[idx, i],
                y = draws_or[idx, i],
                n = n,
                term = term,
                cross = cross,
                wts = wts,
                tmp_idx = tmp_idx,
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

    # if comparison returns a single value, it means we are using a special shortcut comparison function.
    # to do this, we padded with NA. That means we don't want `rowid` or covariates otherwise they will be misleading
    # since misaligned. But we do need the marginaleffects internal columns and by
    if (!all(idx)) {
        if (settings_equal("marginaleffects_safefun_return1", TRUE)) {
            cols <- grep("^estimate$|^group$|^term$|^contrast_?|^marginaleffects_wts_internal$|^by$",
                colnames(out),
                value = TRUE)
            if (isTRUE(checkmate::check_character(by, min.len = 1))) {
                cols <- unique(c(cols, by))
            }
            out <- subset(out, select = cols)
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


compare_hi_lo_bayesian_scalar <- function(out, draws, draws_hi, draws_lo, draws_or, by, cross, variables, fun_list, elasticities, newdata, group_indices) {
    group_data <- vector("list", length(group_indices))
    first_idx <- integer(length(group_indices))
    first_draw <- numeric(length(group_indices))

    for (j in seq_along(group_indices)) {
        idx <- group_indices[[j]]
        n <- length(idx)
        term <- out$term[idx]
        wts <- out$marginaleffects_wts_internal[idx]
        tmp_idx <- out$tmp_idx[idx]
        con <- compare_hi_lo_value(
            hi = draws_hi[idx, 1],
            lo = draws_lo[idx, 1],
            y = draws_or[idx, 1],
            n = n,
            term = term,
            cross = cross,
            wts = wts,
            tmp_idx = tmp_idx,
            newdata = newdata,
            variables = variables,
            fun_list = fun_list,
            elasticities = elasticities
        )
        if (length(con) != 1) {
            return(NULL)
        }
        first_idx[[j]] <- idx[[1]]
        first_draw[[j]] <- con
        group_data[[j]] <- list(
            idx = idx,
            n = n,
            term = term,
            wts = wts,
            tmp_idx = tmp_idx
        )
    }

    draws_scalar <- matrix(NA_real_, nrow = length(group_indices), ncol = ncol(draws))
    draws_scalar[, 1] <- first_draw

    if (ncol(draws_scalar) > 1) {
        for (j in seq_along(group_data)) {
            data <- group_data[[j]]
            for (i in seq.int(2L, ncol(draws_scalar))) {
                con <- compare_hi_lo_value(
                    hi = draws_hi[data$idx, i],
                    lo = draws_lo[data$idx, i],
                    y = draws_or[data$idx, i],
                    n = data$n,
                    term = data$term,
                    cross = cross,
                    wts = data$wts,
                    tmp_idx = data$tmp_idx,
                    newdata = newdata,
                    variables = variables,
                    fun_list = fun_list,
                    elasticities = elasticities
                )
                if (length(con) != 1) {
                    return(NULL)
                }
                draws_scalar[j, i] <- con
            }
        }
    }

    settings_set("marginaleffects_safefun_return1", TRUE)
    cols <- grep("^estimate$|^group$|^term$|^contrast_?|^marginaleffects_wts_internal$|^by$",
        colnames(out),
        value = TRUE)
    if (isTRUE(checkmate::check_character(by, min.len = 1))) {
        cols <- unique(c(cols, by))
    }
    out <- subset(out[first_idx, , drop = FALSE], select = cols)

    FUN_CENTER <- getOption(
        "marginaleffects_posterior_center",
        default = stats::median
    )
    out[, "estimate" := apply(draws_scalar, 1, FUN_CENTER)]

    return(list(out = out, draws = draws_scalar))
}


compare_hi_lo_value <- function(hi, lo, y, n, term, cross, wts, tmp_idx, newdata, variables, fun_list, elasticities) {
    if (n == 0 || length(hi) == 0) {
        return(numeric(0))
    }
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
    con <- try(do_call(fun, args), silent = TRUE)

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
    return(con)
}


compare_hi_lo <- function(hi, lo, y, n, term, cross, wts, tmp_idx, newdata, variables, fun_list, elasticities) {
    con <- compare_hi_lo_value(
        hi = hi,
        lo = lo,
        y = y,
        n = n,
        term = term,
        cross = cross,
        wts = wts,
        tmp_idx = tmp_idx,
        newdata = newdata,
        variables = variables,
        fun_list = fun_list,
        elasticities = elasticities
    )
    if (length(con) == 1) {
        tmp <- rep(NA_real_, length(hi))
        tmp[[1]] <- con
        con <- tmp
        settings_set("marginaleffects_safefun_return1", TRUE)
    }
    return(con)
}



prepare_elasticities <- function(variables, original, out, by, elasticities) {
    FUN <- function(z) {
        (is.character(z$comparison) && z$comparison %in% elasticities) ||
            (is.function(z$comparison) && "x" %in% names(formals(z$comparison)))
    }
    elasticities <- Filter(FUN, variables)

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
    return(elasticities)
}
