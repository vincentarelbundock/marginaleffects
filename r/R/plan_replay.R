# Replay plans support SE computation for transformed or aggregated results.
# First pass: compute estimates and record row groups, weights, and hypotheses.
# SE pass: perturb predictions, replay the recorded plan, then build Jacobians.
# This avoids rerunning the full user-facing pipeline for every perturbation.

sanitize_plan_predict_args <- function(base, extra = list()) {
    out <- utils::modifyList(base %||% list(), extra %||% list())
    # Keep only model-specific predict arguments for replay.
    drop <- c(
        "mfx", "model", "model_perturbed", "hypothesis", "hi", "lo",
        "original", "by", "variables", "cross", "estimates",
        "vcov", "FUN", "index", "numderiv", "J", "newdata", "type",
        "comparison", "calling_function"
    )
    out[setdiff(names(out), drop)]
}

record_plan_aggregation <- function(
    estimates,
    newdata,
    by,
    verbose = TRUE,
    ...) {
    # Record aggregation once so SE replays do not regroup estimates.
    # We use the same data.table `by` groups as the displayed aggregate,
    # but store source row ids and weights for each aggregate row in a list column.
    # Equal-length groups are stacked into matrix blocks for replay with
    # colMeans()/colSums().
    if (is.null(by) || isFALSE(by) || nrow(estimates) <= 1) {
        return(list(out = estimates, agg = NULL))
    }

    missing <- setdiff(setdiff(colnames(by), "by"), colnames(estimates))
    needs_source_id <- length(missing) > 0 || isTRUE(checkmate::check_data_frame(by))
    if (isTRUE(needs_source_id)) {
        estimates <- data.table::copy(estimates)
        # Needed when joins or filters can change row positions.
        plan_id <- ".marginaleffects_plan_est_id"
        while (plan_id %in% colnames(estimates)) {
            plan_id <- paste0(".", plan_id)
        }
        estimates[, (plan_id) := seq_len(.N)]
    }
    estimate_source <- estimates[["estimate"]]

    if (length(missing) > 0) {
        # Bring grouping columns back before computing aggregation groups.
        estimates <- merge_original_data(
            estimates,
            newdata,
            keys = c("rowid", "rowidcf"),
            payload = missing,
            unit_level_only = FALSE
        )
    }

    if (isTRUE(by)) {
        regex <- "^term$|^group$|^contrast$|^contrast_"
        bycols <- grep(regex, colnames(estimates), value = TRUE)
    } else if (isTRUE(checkmate::check_character(by))) {
        bycols <- by
    } else if (isTRUE(checkmate::check_data_frame(by))) {
        idx <- setdiff(intersect(colnames(estimates), colnames(by)), "by")
        by <- harmonize_by_types(estimates, by)
        estimates[by, by := by, on = idx]
        bycols <- "by"
    }

    if ("by" %in% colnames(estimates) && anyNA(estimates[["by"]])) {
        # User-supplied by data can intentionally cover only some rows.
        msg <- insight::format_message(
            "The `by` data.frame does not cover all combinations of response levels and/or predictors. Some estimates will not be included in the aggregation."
        )
        if (isTRUE(verbose)) warning(msg, call. = FALSE)
        estimates <- estimates[!is.na(by), drop = FALSE]
    }

    bycols <- intersect(unique(c("term", bycols)), colnames(estimates))
    weighted <- "marginaleffects_wts_internal" %in% colnames(newdata)

    # Record replay indices with the same groups as the displayed aggregate.
    if (isTRUE(weighted)) {
        groups_dt <- estimates[,
            .(
                idx = list(if (isTRUE(needs_source_id)) get(plan_id) else .I),
                w = list(marginaleffects_wts_internal)
            ),
            keyby = bycols
        ]
    } else {
        groups_dt <- estimates[,
            .(idx = list(if (isTRUE(needs_source_id)) get(plan_id) else .I)),
            keyby = bycols
        ]
    }

    idx <- groups_dt[["idx"]]
    w <- if (isTRUE(weighted)) groups_dt[["w"]] else NULL
    n_groups <- length(idx)
    group_len <- lengths(idx)

    # Equal-size blocks let replay use colSums without ragged padding.
    make_block <- function(cols) {
        n <- group_len[cols][1]
        idx_mat <- matrix(unlist(idx[cols], use.names = FALSE), nrow = n)
        w_mat <- NULL
        if (isTRUE(weighted)) {
            w_mat <- matrix(unlist(w[cols], use.names = FALSE), nrow = n)
        }
        list(cols = cols, idx = idx_mat, w = w_mat)
    }

    if (length(unique(group_len)) == 1L) {
        blocks <- list(make_block(seq_len(n_groups)))
    } else {
        blocks <- lapply(split(seq_len(n_groups), group_len), make_block)
    }

    agg <- list(blocks = blocks, n = n_groups, weighted = weighted)
    out <- groups_dt[, ..bycols]
    out[, estimate := apply_plan_aggregation(agg, estimate_source)]
    return(list(out = out, agg = agg))
}

apply_plan_aggregation <- function(agg, est) {
    out <- numeric(agg$n)

    for (block in agg$blocks) {
        e <- est[block$idx]
        dim(e) <- dim(block$idx)

        if (isTRUE(agg$weighted)) {
            w <- block$w
            missing_estimate <- is.na(e)
            e[missing_estimate] <- 0
            w[missing_estimate] <- 0
            zero_weight <- !is.na(w) & w == 0
            e[zero_weight] <- 0
            out[block$cols] <- colSums(e * w) / colSums(w)
        } else {
            out[block$cols] <- colMeans(e, na.rm = TRUE)
        }
    }

    out
}

apply_plan_aggregation_and_hypothesis <- function(est, agg = NULL, hyp = NULL) {
    # Preserve original pipeline order: aggregate, then transform/test.
    if (!is.null(agg)) {
        est <- apply_plan_aggregation(agg, est)
    }
    if (!is.null(hyp)) {
        est <- hyp$apply(est)
    }
    est
}

validate_plan_replay <- function(kind, baseline, expected) {
    # Guard against stale or incomplete replay plans.
    tolerance <- sqrt(.Machine$double.eps)
    if (!isTRUE(all.equal(baseline, expected, tolerance = tolerance, check.attributes = FALSE))) {
        stop_sprintf("Internal error: %s plan baseline check failed.", kind)
    }
}

plan_std_error <- function(
    built,
    mfx,
    estimates,
    type,
    dots = list(),
    contrast_data = NULL,
    variables = NULL,
    numderiv = NULL,
    unconditional = NULL) {
    if (!is.null(unconditional)) {
        return(plan_unconditional_se(
            built = built,
            mfx = mfx,
            estimates = estimates,
            type = type,
            dots = dots,
            contrast_data = contrast_data,
            variables = variables,
            numderiv = numderiv,
            unconditional = unconditional
        ))
    }

    if ("std.error" %in% colnames(estimates) ||
        (!is.null(mfx) && !is.null(mfx@draws))) {
        return(list(mfx = mfx, estimates = estimates))
    }

    plan <- built$plan
    kind <- plan$kind
    if (!isTRUE(kind %in% c("predictions", "comparisons"))) {
        stop_sprintf("Unknown plan kind: %s", kind %||% "NULL")
    }

    if (
        !isTRUE(checkmate::check_matrix(mfx@vcov_model))) {
        return(list(mfx = mfx, estimates = estimates))
    }

    # Try autodiff first; fall back to numerical delta method.
    ad_args <- list(
        plan = plan,
        mfx = mfx,
        kind = kind,
        type = type,
        vcov = mfx@vcov_model,
        estimate = estimates[["estimate"]]
    )
    if (identical(kind, "comparisons")) {
        ad_args$hi <- contrast_data$hi
        ad_args$lo <- contrast_data$lo
    }
    ad <- do_call(autodiff_try, ad_args)
    if (!is.null(ad)) {
        mfx@jacobian <- ad$jacobian
        estimates[["std.error"]] <- ad$std.error
        return(list(mfx = mfx, estimates = estimates))
    }

    if (identical(kind, "predictions")) {
        # Delta method callback: predict, then replay prediction plan.
        fun <- function(model_perturbed, ...) {
            pred <- prediction_plan_predict(plan, model_perturbed, ...)
            prediction_plan_apply(plan, pred)
        }
        args <- list(
            mfx = mfx,
            model_perturbed = mfx@model,
            vcov = mfx@vcov_model,
            type = type,
            FUN = fun,
            hypothesis = mfx@hypothesis
        )
        args <- utils::modifyList(args, dots)
        se <- do_call(get_se_delta, args)
        if (is.numeric(se) && length(se) == nrow(estimates)) {
            mfx@jacobian <- attr(se, "jacobian")
            estimates[["std.error"]] <- as.vector(se)
        }
    } else {
        # Delta method callback: predict hi/lo, then replay comparison plan.
        fun <- function(model_perturbed, ...) {
            preds <- comparison_plan_predict(plan, model_perturbed, ...)
            comparison_plan_apply(plan, preds$hi, preds$lo, preds$or)
        }
        args <- list(
            mfx = mfx,
            model_perturbed = mfx@model,
            vcov = mfx@vcov_model,
            type = type,
            FUN = fun,
            variables = variables,
            hypothesis = mfx@hypothesis,
            hi = contrast_data$hi,
            lo = contrast_data$lo,
            original = contrast_data$original,
            estimates = estimates,
            numderiv = numderiv
        )
        args <- utils::modifyList(args, dots)
        se <- do_call(get_se_delta, args)
        mfx@jacobian <- attr(se, "jacobian")
        estimates[["std.error"]] <- as.vector(as.numeric(se))
    }

    list(mfx = mfx, estimates = estimates)
}
