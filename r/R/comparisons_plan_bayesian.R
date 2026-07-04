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


comparison_plan_build_bayesian <- function(out, draws, draws_hi, draws_lo, draws_or, by, cross, variables, fun_list, elasticities, newdata) {
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
    list(out = result$out, draws = result$draws, plan = NULL)
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
            out <- subset(out, select = comparison_scalar_columns(out, by))
        }
        out <- out[idx, , drop = FALSE]
    }

    out[, "estimate" := comparison_posterior_center(draws)]

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
    out <- subset(out[first_idx, , drop = FALSE], select = comparison_scalar_columns(out, by))
    out[, "estimate" := comparison_posterior_center(draws_scalar)]

    return(list(out = out, draws = draws_scalar))
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
