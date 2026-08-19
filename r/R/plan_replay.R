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

    # Only a `by` data frame can reorder or drop rows on its way through the
    # resolver's merges and joins; explicit source ids keep the replay
    # indices anchored to the original estimate positions in that case.
    needs_source_id <- isTRUE(checkmate::check_data_frame(by))
    if (isTRUE(needs_source_id)) {
        estimates <- data.table::copy(estimates)
        plan_id <- ".marginaleffects_plan_est_id"
        while (plan_id %in% colnames(estimates)) {
            plan_id <- paste0(".", plan_id)
        }
        estimates[, (plan_id) := seq_len(.N)]
    }
    estimate_source <- estimates[["estimate"]]

    resolved <- resolve_by_rows(estimates, newdata, by, verbose = verbose)
    estimates <- resolved$estimates
    bycols <- resolved$bycols

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

apply_plan_stages <- function(est, agg = NULL, hyp = NULL) {
    # Preserve original pipeline order: aggregate, then transform/test. The
    # intermediate value is returned as well because the hypothesis stage is
    # differentiated at the estimates which feed it, not at its own output.
    if (!is.null(agg)) {
        est <- apply_plan_aggregation(agg, est)
    }
    pre <- est
    if (!is.null(hyp)) {
        est <- hyp$apply(est)
    }
    list(pre = pre, post = est)
}

# Element-wise agreement for replay guards. all.equal() compares a *mean*
# relative difference, under which one badly wrong row hides inside enough
# correct ones; a guard advertised as fail-closed must bound every element.
#
# Missing values are legitimate on some model paths (tidymodels predictions,
# for one), so a missing entry is agreement exactly when it is missing on
# both sides -- the same convention all.equal() applies. A value present on
# one side and missing on the other is disagreement.
plan_replay_agrees <- function(a, b, tolerance = sqrt(.Machine$double.eps)) {
    if (length(a) != length(b)) {
        return(FALSE)
    }
    if (!is.numeric(a) || !is.numeric(b)) {
        # Classification models predict factors or characters. There is no
        # tolerance question for those: agreement is exact equality, which
        # all.equal() checks element by element for non-numeric types.
        return(isTRUE(all.equal(a, b, check.attributes = FALSE)))
    }
    finite_a <- is.finite(a)
    finite_b <- is.finite(b)
    if (!identical(finite_a, finite_b)) {
        return(FALSE)
    }
    # Non-finite entries are compared exactly rather than by subtraction: Inf
    # minus Inf is NaN, which would read as disagreement and raise the generic
    # "plan baseline check failed" error on top of whatever the caller was
    # about to report. A user-supplied `comparison` function returning Inf is a
    # user error with its own message, not a stale replay plan.
    if (!identical(a[!finite_a], b[!finite_b])) {
        return(FALSE)
    }
    a <- a[finite_a]
    b <- b[finite_b]
    delta <- abs(a - b)
    isTRUE(all(delta <= tolerance * pmax(abs(b), 1)))
}


validate_plan_replay <- function(kind, baseline, expected) {
    # Guard against stale or incomplete replay plans.
    if (!plan_replay_agrees(baseline, expected)) {
        stop_sprintf("Internal error: %s plan baseline check failed.", kind)
    }
}


simulation_replay_store <- function(mfx, plan, transforms = list(), enabled = FALSE) {
    if (!isTRUE(enabled) || is.null(plan)) {
        return(mfx)
    }

    # The inference-free estimate path does not need model-matrix caches. Build
    # them once here because simulation replay will reuse them for every draw.
    if (identical(plan$kind, "predictions")) {
        newdata <- plan$predict_args$newdata
        if (is.null(attr(newdata, "marginaleffects_model_matrix"))) {
            plan$predict_args$newdata <- add_model_matrix_attribute_data(mfx, newdata)
        }
    } else if (identical(plan$kind, "comparisons")) {
        if (is.null(attr(plan$predict_args$hi, "marginaleffects_model_matrix"))) {
            plan$predict_args$hi <- add_model_matrix_attribute_data(mfx, plan$predict_args$hi)
        }
        if (is.null(attr(plan$predict_args$lo, "marginaleffects_model_matrix"))) {
            plan$predict_args$lo <- add_model_matrix_attribute_data(mfx, plan$predict_args$lo)
        }
        if (
            isTRUE(plan$need_y) &&
                is.null(attr(plan$predict_args$original, "marginaleffects_model_matrix"))
        ) {
            plan$predict_args$original <- add_model_matrix_attribute_data(
                mfx,
                plan$predict_args$original
            )
        }
    }

    transforms <- Filter(Negate(is.null), transforms)
    attr(mfx, "marginaleffects_simulation_replay") <- list(
        plan = plan,
        transforms = transforms
    )
    mfx
}


simulation_replay_evaluate <- function(replay, model) {
    plan <- replay$plan
    if (identical(plan$kind, "predictions")) {
        pred <- prediction_plan_predict(plan, model)
        estimate <- prediction_plan_apply(plan, pred)
    } else if (identical(plan$kind, "comparisons")) {
        pred <- comparison_plan_predict(plan, model)
        estimate <- comparison_plan_apply(plan, pred$hi, pred$lo, pred$or)
    } else {
        stop_sprintf("Unknown simulation replay plan: %s", plan$kind %||% "NULL")
    }

    for (transform in replay$transforms) {
        if (is.list(transform)) {
            transform <- transform[[1]]
        }
        estimate <- transform(estimate)
    }
    as.vector(estimate)
}


simulation_replay_validate <- function(replay, model, expected) {
    if (is.null(replay)) {
        return(NULL)
    }
    baseline <- tryCatch(
        simulation_replay_evaluate(replay, model),
        error = function(e) NULL
    )
    # Element-wise, like every other replay gate: a mean-relative check would
    # let one badly wrong row hide among enough correct ones.
    if (!plan_replay_agrees(baseline, expected)) {
        return(NULL)
    }
    replay
}


# Wrap a candidate Jacobian in the resolver's return shape. `propagate` is the
# caller's covariance propagation, which doubles as a validity check: a
# derivative whose covariance cannot be formed is not usable, so the candidate
# is refused and the caller falls back to the next one. `safe` distinguishes a
# candidate with a fallback behind it from the last resort, whose errors belong
# to the user.
plan_jacobian_result <- function(J, method, propagate, safe = TRUE) {
    if (is.null(propagate)) {
        return(list(jacobian = J, method = method))
    }
    propagated <- if (isTRUE(safe)) {
        tryCatch(propagate(J), error = function(e) NULL)
    } else {
        propagate(J)
    }
    if (is.null(propagated)) {
        return(NULL)
    }
    list(
        jacobian = propagated$jacobian,
        method = method,
        std.error = propagated$std.error
    )
}


# Differentiate a recorded plan with respect to the model coefficients. This is
# the single entry point for every consumer of a plan Jacobian, so the routing
# between the user's own derivative, the analytic derivative, and numerical
# differentiation is decided in one place.
#
# `propagate` is an optional function of the candidate Jacobian which returns
# the propagated `list(std.error, jacobian)`, or NULL when the candidate cannot
# be propagated; callers which want a Jacobian and nothing else omit it and
# never form a covariance matrix. Returns NULL when no derivative is available
# -- posterior draws, for one -- and otherwise a list with `jacobian` (one row
# per estimate), `method` ("custom", "analytic", "analytic+numeric_stage", or
# "numeric"), and `std.error` when `propagate` was supplied.
compute_plan_jacobian <- function(
    plan,
    mfx,
    estimates,
    type,
    kind,
    dots = list(),
    contrast_data = NULL,
    variables = NULL,
    numderiv = NULL,
    analytic = TRUE,
    propagate = NULL) {
    # Explicit user Jacobians retain priority. Everything else prefers the
    # analytic derivative, which is on by default, and falls back to numerical
    # differentiation when the estimand is not eligible for it.
    custom_jacobian <- settings_get("jacobian_function")

    analytic_enabled <- isTRUE(analytic) &&
        isTRUE(getOption("marginaleffects_analytic_jacobian", default = TRUE))
    if (is.null(custom_jacobian) && analytic_enabled) {
        J <- get_jacobian_analytic(
            model = mfx@model,
            plan = plan,
            kind = kind,
            type = type,
            estimate = estimates[["estimate"]],
            contrast_data = contrast_data
        )
        if (!is.null(J)) {
            method <- if (isTRUE(attr(
                J,
                "marginaleffects_numeric_stage",
                exact = TRUE
            ))) {
                "analytic+numeric_stage"
            } else {
                "analytic"
            }
            out <- plan_jacobian_result(J, method, propagate)
            if (!is.null(out)) {
                return(out)
            }
        }
    }

    # Numerical differentiation, stopping before the hypothesis stage whenever
    # that stage can be differentiated on its own. Differentiating the whole
    # composition forces the finite difference to see an average over many
    # predictions, whose roundoff is then amplified by the inverse of the
    # coefficient-scaled step; composing instead keeps the model evaluations on
    # the well-scaled quantity and reuses the Jacobian rather than discarding
    # it (#1750).
    numeric_jacobian <- function(compose) {
        if (identical(kind, "predictions")) {
            # Delta method callback: predict, then replay prediction plan.
            fun <- function(model_perturbed, ...) {
                pred <- prediction_plan_predict(plan, model_perturbed, ...)
                stages <- prediction_plan_apply_stages(plan, pred)
                if (isTRUE(compose)) stages$pre else stages$post
            }
            args <- list(
                mfx = mfx,
                model_perturbed = mfx@model,
                type = type,
                FUN = fun,
                hypothesis = if (isTRUE(compose)) NULL else mfx@hypothesis
            )
        } else {
            # Delta method callback: predict hi/lo, then replay comparison plan.
            fun <- function(model_perturbed, ...) {
                preds <- comparison_plan_predict(plan, model_perturbed, ...)
                stages <- comparison_plan_apply_stages(
                    plan,
                    preds$hi,
                    preds$lo,
                    preds$or
                )
                if (isTRUE(compose)) stages$pre else stages$post
            }
            args <- list(
                mfx = mfx,
                model_perturbed = mfx@model,
                type = type,
                FUN = fun,
                variables = variables,
                hypothesis = if (isTRUE(compose)) NULL else mfx@hypothesis,
                hi = contrast_data$hi,
                lo = contrast_data$lo,
                original = contrast_data$original,
                estimates = estimates,
                numderiv = numderiv
            )
        }
        args <- utils::modifyList(args, dots)
        do_call(get_delta_jacobian, args)
    }

    hyp <- plan$hyp
    # `stage_pull` maps the pre-hypothesis Jacobian to the post-hypothesis
    # one. Exact when the stage has a compiled matrix (an affine map's
    # derivative does not depend on its offset -- probing the offset
    # numerically cancels catastrophically) or a structured pullback (the
    # centering shortcuts, whose operators are dense as matrices); a
    # central-difference probe of the stage arithmetic otherwise.
    stage_pull <- NULL
    # A user-supplied Jacobian function is authoritative and already covers the
    # whole pipeline, hypothesis included, so it is never composed with.
    if (!is.null(hyp) && is.null(custom_jacobian)) {
        exact <- hypothesis_stage_exact(hyp, n_post = nrow(estimates))
        # Recovering the pre-hypothesis estimates replays the plan, so it is
        # only worth doing when the probe is the sole remaining option.
        estimate_pre <- if (exact) {
            NULL
        } else {
            plan_replay_estimate_pre(
                plan = plan,
                kind = kind,
                estimate = estimates[["estimate"]]
            )
        }
        if (exact || !is.null(estimate_pre)) {
            stage_pull <- function(J) {
                res <- hypothesis_stage_pullback(hyp, J, at = estimate_pre)
                if (is.null(res)) NULL else res$jacobian
            }
        }
    }

    jac <- numeric_jacobian(!is.null(stage_pull))
    if (is.null(jac)) {
        return(NULL)
    }

    if (!is.null(stage_pull)) {
        # A dimension mismatch errors inside the pull and lands here as NULL,
        # which the fail-closed branch below turns into a redo.
        composed <- tryCatch(
            as.matrix(stage_pull(jac$jacobian)),
            error = function(e) NULL
        )
        if (!is.null(composed) && nrow(composed) == nrow(estimates)) {
            # The inner derivative's provenance survives the composition: the
            # pull only rescales the stage a custom or numeric J produced.
            out <- plan_jacobian_result(composed, jac$method, propagate)
            if (!is.null(out)) {
                return(out)
            }
        }
        # Fail closed. A partially composed result would be silently wrong, so
        # an unusable composition redoes the original whole-pipeline
        # derivative. The repeat is inherent, not an optimization miss: the
        # fallback differentiates through the hypothesis stage by re-running
        # the model, which the pre-hypothesis Jacobian computed above cannot
        # supply once the pull is unusable.
        jac <- numeric_jacobian(FALSE)
        if (is.null(jac)) {
            return(NULL)
        }
    }

    plan_jacobian_result(jac$jacobian, jac$method, propagate, safe = FALSE)
}


plan_std_error <- function(
    built,
    mfx,
    estimates,
    type,
    vcov = NULL,
    dots = list(),
    contrast_data = NULL,
    variables = NULL,
    numderiv = NULL) {
    if (inherits(vcov, "marginaleffects_vcov_unconditional")) {
        return(plan_unconditional_se(
            built = built,
            mfx = mfx,
            estimates = estimates,
            type = type,
            dots = dots,
            contrast_data = contrast_data,
            variables = variables,
            numderiv = numderiv,
            unconditional = vcov
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

    jac <- compute_plan_jacobian(
        plan = plan,
        mfx = mfx,
        estimates = estimates,
        type = type,
        kind = kind,
        dots = dots,
        contrast_data = contrast_data,
        variables = variables,
        numderiv = numderiv,
        propagate = function(J) {
            # Model-specific arguments which no formal upstream absorbed must
            # reach get_coef() during vcov alignment: gamlss models, for one,
            # cannot extract coefficients without their `what` argument.
            extra <- dots[setdiff(
                names(dots),
                c("", names(formals(get_delta_jacobian)))
            )]
            do_call(
                std_error_from_jacobian,
                c(list(J, mfx@vcov_model, mfx@model), extra)
            )
        }
    )
    se <- jac$std.error

    # Provenance records what actually produced the stored matrix, not which
    # options were set: a custom jacobian function which returned NULL fell
    # back to numerical differentiation and must say so.
    mfx@jacobian_method <- jac$method %||% "numeric"

    # A custom Jacobian is honored, not silently recycled or dropped: if its
    # row count does not match the estimates it claims to differentiate, the
    # standard errors it implies are meaningless.
    if (
        identical(mfx@jacobian_method, "custom") &&
            (!is.numeric(se) || length(se) != nrow(estimates))
    ) {
        stop_sprintf(
            "The matrix returned by the `marginaleffects_jacobian_function` option has %s row(s), but there are %s estimates.",
            length(se),
            nrow(estimates)
        )
    }

    # Same guard for both kinds: an SE vector whose length does not match the
    # estimates must not be assigned, because data.table recycling would
    # silently repeat a scalar across every row.
    if (is.numeric(se) && length(se) == nrow(estimates)) {
        mfx@jacobian <- jac$jacobian
        estimates[["std.error"]] <- as.vector(se)
    }

    list(mfx = mfx, estimates = estimates)
}


# Recover the estimates which feed the hypothesis stage, using the predictions
# recorded when the plan was built. Fails closed: the recovered stage output
# must reproduce the reported estimates, otherwise the plan is stale or the
# pipeline needs a model evaluation this shortcut cannot supply.
plan_replay_estimate_pre <- function(plan, kind, estimate) {
    if (plan_groups_use_y(plan)) {
        return(NULL)
    }
    stages <- tryCatch(
        {
            if (identical(kind, "predictions")) {
                prediction_plan_apply_stages(plan, plan$baseline_prediction)
            } else {
                comparison_plan_apply_stages(
                    plan,
                    plan$baseline_hi,
                    plan$baseline_lo
                )
            }
        },
        error = function(e) NULL
    )
    if (is.null(stages) || !is.numeric(stages$pre)) {
        return(NULL)
    }
    # Element-wise, like every other replay gate: a mean-relative check would
    # let one badly wrong row hide among enough correct ones.
    if (!plan_replay_agrees(stages$post, estimate)) {
        return(NULL)
    }
    stages$pre
}
