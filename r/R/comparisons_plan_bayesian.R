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
    if (length(by_idx_unique) == 0L) {
        return(list())
    }

    group <- match(by_idx, by_idx_unique)
    out <- unname(split(seq_along(by_idx), group))
    names(out) <- as.character(by_idx_unique)
    return(out)
}


comparison_group_calls <- function(out, group_indices, context) {
    lapply(group_indices, function(idx) {
        term <- out$term[idx]
        wts <- out$marginaleffects_wts_internal[idx]
        tmp_idx <- out$tmp_idx[idx]
        call <- comparison_call_args(
            term = term,
            wts = wts,
            tmp_idx = tmp_idx,
            context = context
        )
        call$idx <- idx
        call$n <- length(idx)
        call
    })
}


comparison_call_value <- function(call, hi, lo, y) {
    if (call$n == 0 || length(hi) == 0) {
        return(numeric(0))
    }

    value_args <- call$args
    value_args$hi <- hi
    value_args$lo <- lo
    if (isTRUE(call$uses_y)) {
        value_args$y <- y
    }
    con <- try(do_call(call$fun, value_args), silent = TRUE)
    comparison_validate_result(con, call$n)
    con
}


comparison_fast_path_vector_key <- function(call) {
    isTRUE(call$fun_key %in% c("difference", "ratio", "lnratio", "lift"))
}


comparison_fast_path_vector <- function(call, hi, lo) {
    if (!comparison_fast_path_vector_key(call)) return(NULL)
    out <- switch(
        call$fun_key,
        "difference" = hi - lo,
        "ratio" = hi / lo,
        "lnratio" = log(hi / lo),
        "lift" = (hi - lo) / lo
    )
    if (anyNA(out)) {
        idx <- which(colSums(is.na(out)) > 0L)[[1]]
        comparison_validate_result(out[, idx], call$n)
    }
    out
}


comparison_fast_path_scalar <- function(call, hi, lo) {
    if (!isTRUE(call$fun_key %in% c("differenceavg", "ratioavg", "lnratioavg", "liftavg"))) {
        return(NULL)
    }
    out <- switch(
        call$fun_key,
        "differenceavg" = colMeans(hi - lo),
        "ratioavg" = colMeans(hi) / colMeans(lo),
        "lnratioavg" = log(colMeans(hi) / colMeans(lo)),
        "liftavg" = colMeans(hi - lo) / colMeans(lo)
    )
    if (anyNA(out)) {
        comparison_validate_result(out[[which(is.na(out))[[1]]]], call$n)
    }
    out
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
    posterior_draws_center(draws, FUN_CENTER)
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
    group_calls <- comparison_group_calls(out, group_indices, context)
    scalar_result <- compare_hi_lo_bayesian_scalar(
        out = out,
        draws = draws,
        draws_hi = draws_hi,
        draws_lo = draws_lo,
        draws_or = draws_or,
        by = by,
        context = context,
        group_indices = group_indices,
        group_calls = group_calls
    )
    if (!is.null(scalar_result)) {
        return(scalar_result)
    }

    for (call in group_calls) {
        idx <- call$idx
        fast <- comparison_fast_path_vector(
            call = call,
            hi = draws_hi[idx, , drop = FALSE],
            lo = draws_lo[idx, , drop = FALSE]
        )
        if (is.null(fast)) {
            for (i in seq_len(ncol(draws))) {
                draws[idx, i] <- compare_hi_lo(
                    call = call,
                    hi = draws_hi[idx, i],
                    lo = draws_lo[idx, i],
                    y = draws_or[idx, i]
                )
            }
        } else {
            draws[idx, ] <- fast
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


compare_hi_lo_bayesian_scalar <- function(
    out,
    draws,
    draws_hi,
    draws_lo,
    draws_or,
    by,
    context,
    group_indices,
    group_calls = NULL) {
    if (is.null(group_calls)) {
        group_calls <- comparison_group_calls(out, group_indices, context)
    }
    draws_scalar <- matrix(NA_real_, nrow = length(group_calls), ncol = ncol(draws))

    for (j in seq_along(group_calls)) {
        call <- group_calls[[j]]
        idx <- call$idx
        fast <- comparison_fast_path_scalar(
            call = call,
            hi = draws_hi[idx, , drop = FALSE],
            lo = draws_lo[idx, , drop = FALSE]
        )
        if (!is.null(fast)) {
            draws_scalar[j, ] <- fast
            next
        }
        if (comparison_fast_path_vector_key(call)) {
            return(NULL)
        }

        for (i in seq_len(ncol(draws))) {
            con <- comparison_call_value(
                call = call,
                hi = draws_hi[idx, i],
                lo = draws_lo[idx, i],
                y = draws_or[idx, i]
            )
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


compare_hi_lo <- function(call, hi, lo, y) {
    con <- comparison_call_value(
        call = call,
        hi = hi,
        lo = lo,
        y = y
    )
    if (length(con) == 1) {
        tmp <- rep(NA_real_, length(hi))
        tmp[[1]] <- con
        con <- tmp
        settings_set("marginaleffects_safefun_return1", TRUE)
    }
    return(con)
}
