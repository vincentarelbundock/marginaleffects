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


comparison_plan_record_groups <- function(
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
    need_y,
    point = NULL,
    preserve_order = FALSE) {
    # `preserve_order = TRUE` keeps output rows in the order of their first
    # source row instead of sorting them by group. Posterior-draws models
    # have always reported comparisons in that order.
    #
    # `point` optionally supplies the hi/lo/y vectors (aligned with the rows
    # of `out`) on which the plan is recorded. Models with posterior draws use
    # it to record the plan on a summary of the draws, because their
    # `predicted_*` columns are not meaningful point predictions.
    if (is.null(point)) {
        point <- list(hi = out$predicted_hi, lo = out$predicted_lo, y = out$predicted)
    }
    point_rows <- seq_len(nrow(out))

    # Both operands decide missingness symmetrically: a finite lo with a
    # missing hi is just as uncomputable as the reverse, and letting it
    # through surfaced later as a misleading complaint about the user's
    # comparison function.
    keep_mask <- !is.na(out$predicted_lo) & !is.na(out$predicted_hi)
    na_keep <- if (all(keep_mask)) NULL else which(keep_mask)
    if (!is.null(na_keep)) {
        out <- out[na_keep]
        point_rows <- point_rows[na_keep]
    }

    idx <- intersect(idx, colnames(out))
    n_out <- nrow(out)
    if (length(idx) > 0) {
        groups_dt <- if (isTRUE(preserve_order)) {
            out[, .(rows = list(.I)), by = idx]
        } else {
            out[, .(rows = list(.I)), keyby = idx]
        }
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
    point_rows <- point_rows[perm]
    point_hi <- point$hi[point_rows]
    point_lo <- point$lo[point_rows]
    point_y <- if (is.null(point$y)) NULL else point$y[point_rows]
    perm_store <- if (identical(perm, seq_len(n_out))) NULL else perm

    plan_groups <- vector("list", nrow(bounds))
    out_rows <- vector("list", nrow(bounds))
    estimates <- vector("list", nrow(bounds))
    out_wts <- vector("list", nrow(bounds))
    n_comp <- 0L
    any_scalar_aggregate <- FALSE

    for (j in seq_len(nrow(bounds))) {
        rows <- seq.int(bounds$first[[j]], bounds$last[[j]])
        n <- length(rows)
        term <- out_sorted$term[rows]
        group_wts <- out_sorted$marginaleffects_wts_internal[rows]
        call <- comparison_call(
            hi = point_hi[rows],
            lo = point_lo[rows],
            y = point_y[rows],
            n = n,
            term = term,
            wts = group_wts,
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
            # The comparison consumed the unit-level weights and its output
            # row stands for the whole group. Any further aggregation must
            # weight that row by the group's total weight -- the pooled
            # weighted mean -- never by the stale weight of whichever source
            # row happened to come first, which is 0/0 = NaN when that weight
            # is zero. Zero weights are routine in ATT/matching workflows.
            out_wts[[j]] <- sum(group_wts)
            n_comp <- n_comp + 1L
        } else {
            out_idx <- seq.int(n_comp + 1L, n_comp + n)
            out_rows[[j]] <- rows
            estimates[[j]] <- con
            out_wts[[j]] <- group_wts
            n_comp <- n_comp + n
        }
        plan_groups[[j]] <- list(
            idx = rows,
            n = n,
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
        if ("marginaleffects_wts_internal" %in% colnames(out)) {
            out[,
                marginaleffects_wts_internal := unlist(out_wts, use.names = FALSE)
            ]
        }
    } else {
        out <- out_sorted
    }
    out[, estimate := unlist(estimates, use.names = FALSE)]

    order_store <- NULL
    if (isTRUE(preserve_order)) {
        ord <- order(perm[unlist(out_rows, use.names = FALSE)])
        if (!identical(ord, seq_along(ord))) {
            out <- out[ord]
            order_store <- ord
        }
    }
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
        order = order_store,
        est_keep = est_keep,
        agg = NULL,
        hyp = NULL
    )
    list(out = out, plan = plan)
}


comparison_plan_apply <- function(plan, hi, lo, y = NULL) {
    comparison_plan_apply_stages(plan, hi, lo, y = y)$post
}


# Comparison stage of the plan: per-group comparison functions with the
# recorded missing-value, permutation, and missing-estimate index steps. `hi`,
# `lo`, and `y` are vectors of point predictions or n_pred x k matrices of
# draws; the result has one element (row) per comparison estimate.
comparison_plan_apply_groups <- function(plan, hi, lo, y = NULL) {
    stopifnot(plan_nrow(hi) == plan$n_pred)
    stopifnot(plan_nrow(lo) == plan$n_pred)
    rows <- seq_len(plan$n_pred)
    if (!is.null(plan$na_keep)) {
        rows <- rows[plan$na_keep]
    }
    if (!is.null(plan$perm)) {
        rows <- rows[plan$perm]
    }
    if (!identical(rows, seq_len(plan$n_pred))) {
        hi <- plan_rows(hi, rows)
        lo <- plan_rows(lo, rows)
        if (!is.null(y)) y <- plan_rows(y, rows)
    }

    if (is.matrix(hi)) {
        est <- matrix(NA_real_, nrow = plan$n_comp, ncol = ncol(hi))
        for (g in plan$groups) {
            est[g$out_idx, ] <- comparison_plan_group_eval_draws(g, hi = hi, lo = lo, y = y)
        }
    } else {
        est <- numeric(plan$n_comp)
        for (g in plan$groups) {
            est[g$out_idx] <- comparison_plan_group_eval(g, hi = hi, lo = lo, y = y)
        }
    }
    if (!is.null(plan$order)) {
        est <- plan_rows(est, plan$order)
    }
    if (!is.null(plan$est_keep)) {
        est <- plan_rows(est, plan$est_keep)
    }
    est
}


comparison_plan_apply_stages <- function(plan, hi, lo, y = NULL) {
    est <- comparison_plan_apply_groups(plan, hi, lo, y = y)
    apply_plan_stages(est, plan$agg, plan$hyp)
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


# Apply one comparison-plan group's function to its slice of `hi`, `lo`, and
# (optionally) `y`, checking that the output has the planned length.
comparison_plan_group_eval <- function(g, hi, lo, y) {
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
    con
}
