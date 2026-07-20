predictions_hi_lo_frequentist <- function(model, lo, hi, type, ...) {
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

    list(pred_lo = pred_lo, pred_hi = pred_hi)
}


comparison_plan_build_frequentist <- function(
    out,
    idx,
    context,
    n_pred,
    baseline_hi,
    baseline_lo,
    eta_hi,
    eta_lo,
    model_matrix_used,
    type,
    dots,
    hi,
    lo,
    original,
    need_y) {
    na_keep <- if (anyNA(out$predicted_lo)) which(!is.na(out$predicted_lo)) else NULL
    if (!is.null(na_keep)) {
        out <- out[na_keep]
    }

    idx <- intersect(idx, colnames(out))
    n_out <- nrow(out)
    if (length(idx) > 0) {
        groups_dt <- out[, .(rows = list(.I)), keyby = idx]
        perm <- unlist(groups_dt$rows, use.names = FALSE)
        group_len <- lengths(groups_dt$rows)
        last <- cumsum(group_len)
        bounds <- data.table::data.table(
            first = last - group_len + 1L,
            last = last
        )
    } else {
        perm <- seq_len(n_out)
        bounds <- data.table::data.table(first = 1L, last = n_out)
    }
    out_sorted <- out[perm]
    perm_store <- if (identical(perm, seq_len(n_out))) NULL else perm

    plan_groups <- vector("list", nrow(bounds))
    out_rows <- vector("list", nrow(bounds))
    estimates <- vector("list", nrow(bounds))
    n_comp <- 0L
    any_scalar_aggregate <- FALSE

    for (j in seq_len(nrow(bounds))) {
        rows <- seq.int(bounds$first[[j]], bounds$last[[j]])
        n <- length(rows)
        term <- out_sorted$term[rows]
        call <- comparison_call(
            hi = out_sorted$predicted_hi[rows],
            lo = out_sorted$predicted_lo[rows],
            y = out_sorted$predicted[rows],
            n = n,
            term = term,
            wts = out_sorted$marginaleffects_wts_internal[rows],
            tmp_idx = out_sorted$tmp_idx[rows],
            context = context
        )
        con <- call$value

        if (length(con) == 1) {
            if (n > 1) {
                any_scalar_aggregate <- TRUE
            }
            out_idx <- n_comp + 1L
            out_rows[[j]] <- rows[[1]]
            estimates[[j]] <- con
            n_comp <- n_comp + 1L
        } else {
            out_idx <- seq.int(n_comp + 1L, n_comp + n)
            out_rows[[j]] <- rows
            estimates[[j]] <- con
            n_comp <- n_comp + n
        }
        plan_groups[[j]] <- list(
            idx = rows,
            out_idx = out_idx,
            scalar = length(con) == 1,
            uses_y = call$uses_y,
            fun_key = call$fun_key,
            fun = call$fun,
            args = call$args
        )
    }

    if (isTRUE(any_scalar_aggregate)) {
        out <- out_sorted[unlist(out_rows, use.names = FALSE)]
    } else {
        out <- out_sorted
    }
    out[, estimate := unlist(estimates, use.names = FALSE)]
    out[, tmp_idx := NULL]

    if (isTRUE(any_scalar_aggregate)) {
        keep_cols <- c(
            idx,
            grep(
                "^estimate$|^contrast|^group$|^term$|^marginaleffects_wts_internal$",
                colnames(out),
                value = TRUE
            )
        )
        keep_cols <- unique(intersect(keep_cols, colnames(out)))
        out <- subset(out, select = keep_cols)
    }

    est_keep <- if (anyNA(out$estimate)) which(!is.na(out$estimate)) else NULL
    if (!is.null(est_keep)) {
        out <- out[est_keep, drop = FALSE]
    }

    plan <- list(
        kind = "comparisons",
        n_pred = n_pred,
        baseline_hi = baseline_hi,
        baseline_lo = baseline_lo,
        eta_hi = eta_hi,
        eta_lo = eta_lo,
        model_matrix_used = model_matrix_used,
        need_y = need_y,
        predict_args = list(
            type = type,
            hi = hi,
            lo = lo,
            original = original,
            dots = dots
        ),
        na_keep = na_keep,
        perm = perm_store,
        groups = plan_groups,
        n_comp = n_comp,
        est_keep = est_keep,
        agg = NULL,
        hyp = NULL
    )
    list(out = out, plan = plan)
}


comparison_plan_apply <- function(plan, hi, lo, y = NULL) {
    stopifnot(length(hi) == plan$n_pred)
    stopifnot(length(lo) == plan$n_pred)
    if (!is.null(plan$na_keep)) {
        hi <- hi[plan$na_keep]
        lo <- lo[plan$na_keep]
        if (!is.null(y)) y <- y[plan$na_keep]
    }
    if (!is.null(plan$perm)) {
        hi <- hi[plan$perm]
        lo <- lo[plan$perm]
        if (!is.null(y)) y <- y[plan$perm]
    }

    est <- numeric(plan$n_comp)
    for (g in plan$groups) {
        args <- g$args
        args$hi <- hi[g$idx]
        args$lo <- lo[g$idx]
        if (isTRUE(g$uses_y)) {
            args$y <- y[g$idx]
        }
        con <- do_call(g$fun, args)
        if (length(con) != length(g$out_idx)) {
            stop_sprintf("Internal error: comparison plan group changed shape.")
        }
        est[g$out_idx] <- con
    }
    if (!is.null(plan$est_keep)) {
        est <- est[plan$est_keep]
    }
    apply_plan_aggregation_and_hypothesis(est, plan$agg, plan$hyp)
}


comparison_plan_predict <- function(.plan, model_perturbed, ...) {
    dots <- sanitize_plan_predict_args(.plan$predict_args$dots, list(...))
    args <- c(
        list(
            model = model_perturbed,
            type = .plan$predict_args$type,
            newdata = NULL
        ),
        dots
    )
    args$newdata <- .plan$predict_args$hi
    pred_hi <- do_call(get_predict, args)
    args$newdata <- .plan$predict_args$lo
    pred_lo <- do_call(get_predict, args)
    pred_or <- NULL
    if (isTRUE(.plan$need_y)) {
        args$newdata <- .plan$predict_args$original
        pred_or <- do_call(get_predict, args)
        pred_or <- pred_or[["estimate"]]
    }
    list(
        hi = pred_hi[["estimate"]],
        lo = pred_lo[["estimate"]],
        or = pred_or
    )
}
