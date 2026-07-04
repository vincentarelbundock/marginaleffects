predictions_hi_lo_bayesian <- function(model, lo, hi, type, ...) {
    # brms models need to be combined to use a single seed when sample_new_levels="gaussian"
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

    list(pred_lo = pred_lo, pred_hi = pred_hi)
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


comparison_scalar_columns <- function(out, by) {
    cols <- grep(
        "^estimate$|^group$|^term$|^contrast_?|^marginaleffects_wts_internal$|^by$",
        colnames(out),
        value = TRUE
    )
    if (isTRUE(checkmate::check_character(by, min.len = 1))) {
        cols <- unique(c(cols, by))
    }
    cols
}


comparison_posterior_center <- function(draws) {
    FUN_CENTER <- getOption(
        "marginaleffects_posterior_center",
        default = stats::median
    )
    apply(draws, 1, FUN_CENTER)
}


compare_hi_lo_bayesian <- function(out, draws, draws_hi, draws_lo, draws_or, by, context) {
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
        context = context,
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
                wts = wts,
                tmp_idx = tmp_idx,
                context = context
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
            out <- subset(out, select = comparison_scalar_columns(out, by))
        }
        out <- out[idx, , drop = FALSE]
    }

    out[, "estimate" := comparison_posterior_center(draws)]

    return(list(out = out, draws = draws))
}


compare_hi_lo_bayesian_scalar <- function(out, draws, draws_hi, draws_lo, draws_or, by, context, group_indices) {
    draws_scalar <- matrix(NA_real_, nrow = length(group_indices), ncol = ncol(draws))

    for (j in seq_along(group_indices)) {
        idx <- group_indices[[j]]
        n <- length(idx)
        term <- out$term[idx]
        wts <- out$marginaleffects_wts_internal[idx]
        tmp_idx <- out$tmp_idx[idx]

        for (i in seq_len(ncol(draws))) {
            con <- comparison_call(
                hi = draws_hi[idx, i],
                lo = draws_lo[idx, i],
                y = draws_or[idx, i],
                n = n,
                term = term,
                wts = wts,
                tmp_idx = tmp_idx,
                context = context
            )$value
            if (length(con) != 1) {
                return(NULL)
            }
            draws_scalar[j, i] <- con
        }
    }

    first_idx <- vapply(group_indices, `[[`, integer(1), 1L)
    settings_set("marginaleffects_safefun_return1", TRUE)
    out <- subset(out[first_idx, , drop = FALSE], select = comparison_scalar_columns(out, by))
    out[, "estimate" := comparison_posterior_center(draws_scalar)]

    return(list(out = out, draws = draws_scalar))
}


compare_hi_lo <- function(hi, lo, y, n, term, wts, tmp_idx, context) {
    con <- comparison_call(
        hi = hi,
        lo = lo,
        y = y,
        n = n,
        term = term,
        wts = wts,
        tmp_idx = tmp_idx,
        context = context
    )$value
    if (length(con) == 1) {
        tmp <- rep(NA_real_, length(hi))
        tmp[[1]] <- con
        con <- tmp
        settings_set("marginaleffects_safefun_return1", TRUE)
    }
    return(con)
}
