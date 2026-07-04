predictions_hi_lo_frequentist <- function(model, lo, hi, type, ...) {
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

    list(pred_lo = pred_lo, pred_hi = pred_hi)
}


comparison_plan_build_frequentist <- function(
    out,
    idx,
    cross,
    variables,
    fun_list,
    elasticities,
    newdata,
    pred_hi,
    pred_lo,
    pred_or,
    type,
    dots,
    hi,
    lo,
    original,
    need_y) {
    n_pred <- length(pred_lo)
    na_keep <- if (anyNA(out$predicted_lo)) which(!is.na(out$predicted_lo)) else NULL
    if (!is.null(na_keep)) {
        out <- out[na_keep]
    }

    idx <- intersect(idx, colnames(out))
    out[, marginaleffects_plan_row_id := seq_len(.N)]
    if (length(idx) > 0) {
        groups_dt <- out[, .(rows = list(marginaleffects_plan_row_id)), keyby = idx]
        perm <- unlist(groups_dt$rows, use.names = FALSE)
    } else {
        perm <- seq_len(nrow(out))
    }
    out_sorted <- out[perm]
    perm_store <- if (identical(perm, seq_len(nrow(out)))) NULL else perm

    if (length(idx) > 0) {
        bounds <- out_sorted[, .(first = .I[1], last = .I[.N]), keyby = idx]
    } else {
        bounds <- data.table::data.table(first = 1L, last = nrow(out_sorted))
    }

    plan_groups <- vector("list", nrow(bounds))
    out_parts <- vector("list", nrow(bounds))
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
            cross = cross,
            wts = out_sorted$marginaleffects_wts_internal[rows],
            tmp_idx = out_sorted$tmp_idx[rows],
            newdata = newdata,
            variables = variables,
            fun_list = fun_list,
            elasticities = elasticities
        )
        con <- call$value

        if (length(con) == 1) {
            if (n > 1) {
                any_scalar_aggregate <- TRUE
            }
            out_idx <- n_comp + 1L
            part <- out_sorted[rows[[1]], , drop = FALSE]
            part[, estimate := con]
            n_comp <- n_comp + 1L
        } else {
            out_idx <- seq.int(n_comp + 1L, n_comp + n)
            part <- out_sorted[rows, , drop = FALSE]
            part[, estimate := con]
            n_comp <- n_comp + n
        }
        out_parts[[j]] <- part
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

    out <- data.table::rbindlist(out_parts, fill = TRUE)
    out[, tmp_idx := NULL]
    out[, marginaleffects_plan_row_id := NULL]

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
        n_pred = n_pred,
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
        est[g$out_idx] <- do_call(g$fun, args)
    }
    if (!is.null(plan$est_keep)) {
        est <- est[plan$est_keep]
    }
    apply_plan_aggregation_and_hypothesis(est, plan$agg, plan$hyp)
}


comparison_plan_predict <- function(.plan, model_perturbed, ...) {
    dots <- sanitize_plan_predict_args(.plan$predict_args$dots, list(...))
    args_hi <- c(
        list(
            model = model_perturbed,
            type = .plan$predict_args$type,
            newdata = .plan$predict_args$hi
        ),
        dots
    )
    args_lo <- c(
        list(
            model = model_perturbed,
            type = .plan$predict_args$type,
            newdata = .plan$predict_args$lo
        ),
        dots
    )
    pred_hi <- do_call(get_predict, args_hi)
    pred_lo <- do_call(get_predict, args_lo)
    pred_or <- NULL
    if (isTRUE(.plan$need_y)) {
        args_or <- c(
            list(
                model = model_perturbed,
                type = .plan$predict_args$type,
                newdata = .plan$predict_args$original
            ),
            dots
        )
        pred_or <- do_call(get_predict, args_or)
        pred_or <- pred_or[["estimate"]]
    }
    list(
        hi = pred_hi[["estimate"]],
        lo = pred_lo[["estimate"]],
        or = pred_or
    )
}
