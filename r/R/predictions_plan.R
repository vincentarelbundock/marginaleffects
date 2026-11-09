prediction_prepare_newdata <- function(mfx, variables = NULL) {
    # analogous to comparisons(variables=list(...))
    if (!is.null(variables)) {
        mfx <- add_variables(
            variables = variables,
            mfx = mfx
        )
        args <- list(
            model = mfx@model,
            newdata = mfx@newdata,
            grid_type = "counterfactual",
            marginaleffects_internal = mfx
        )
        for (v in mfx@variables) {
            args[[v$name]] <- v$value
        }
        mfx@newdata <- do.call("datagrid", args)
    }

    if (!"rowid" %in% colnames(mfx@newdata)) {
        mfx@newdata[["rowid"]] <- seq_len(nrow(mfx@newdata))
    }

    mfx
}


prediction_plan_build <- function(
    mfx,
    type,
    model_perturbed = NULL,
    by = NULL,
    hypothesis = NULL,
    verbose = TRUE,
    ...) {
    dots <- list(...)
    newdata <- mfx@newdata
    model <- if (is.null(model_perturbed)) mfx@model else model_perturbed

    out <- get_predict_error(
        model,
        newdata = newdata,
        type = type,
        mfx = mfx,
        ...
    )
    linear_predictor <- attr(out, "marginaleffects_linear_predictor")
    model_matrix_used <- isTRUE(attr(out, "marginaleffects_model_matrix_used"))
    data.table::setDT(out)

    if (
        !"rowid" %in% colnames(out) &&
            "rowid" %in% colnames(newdata) &&
            nrow(out) == nrow(newdata)
    ) {
        out$rowid <- newdata$rowid
    }

    draws <- attr(out, "posterior_draws")
    raw_estimate <- out[["estimate"]]
    # Posterior draws (n_pred x ndraws) are replayed through the same plan
    # as the point estimates, including the `keep` step which drops padding.
    raw_draws <- draws

    keep <- NULL
    if ("rowid" %in% colnames(out)) {
        idx_keep <- out$rowid > 0
        if (!all(idx_keep)) {
            keep <- which(idx_keep)
        }
    }

    tmp <- unpad(out, draws)
    out <- tmp$out
    draws <- tmp$draws

    payload <- NULL
    if (!isTRUE(checkmate::check_function(hypothesis))) {
        payload <- c("rowidcf", "marginaleffects_wts_internal")
        if (isTRUE(checkmate::check_character(by))) {
            payload <- c(payload, by)
        } else if (isTRUE(checkmate::check_data_frame(by))) {
            payload <- c(payload, setdiff(intersect(colnames(newdata), colnames(by)), "by"))
        }
        if (isTRUE(checkmate::check_formula(hypothesis))) {
            form <- sanitize_hypothesis_formula(hypothesis)
            payload <- c(payload, form$group, mfx@variable_names_datagrid)
        }
        payload <- unique(payload)
    }

    out <- merge_original_data(
        out,
        newdata,
        payload = payload,
        unit_level_only = FALSE
    )

    by <- sanitize_by(mfx, by, out = out, implicit = "group")

    prediction_plan_record(
        out = out,
        raw_estimate = raw_estimate,
        raw_draws = raw_draws,
        linear_predictor = linear_predictor,
        model_matrix_used = model_matrix_used,
        keep = keep,
        newdata = newdata,
        type = type,
        mfx = mfx,
        dots = dots,
        by = by,
        hypothesis = hypothesis,
        verbose = verbose,
        ...
    )
}


# Record the prediction plan on point estimates, then push posterior draws
# (when the model supplies them) through the very same stages. There is a
# single plan for point estimates and Bayesian draws.
prediction_plan_record <- function(
    out,
    raw_estimate,
    raw_draws,
    linear_predictor,
    model_matrix_used,
    keep,
    newdata,
    type,
    mfx,
    dots,
    by = NULL,
    hypothesis = NULL,
    verbose = TRUE,
    ...) {
    data.table::setattr(out, "posterior_draws", NULL)
    draws <- if (is.null(raw_draws) || is.null(keep)) {
        raw_draws
    } else {
        raw_draws[keep, , drop = FALSE]
    }

    agg <- record_plan_aggregation(
        out,
        newdata = newdata,
        by = by,
        verbose = verbose,
        ...
    )
    out <- agg$out
    tmp <- plan_draws_aggregate(out, draws, agg$agg)
    out <- tmp$out
    draws <- tmp$draws

    plan_draws_check_hypothesis(hypothesis, draws)
    hyp <- hypothesis_compile(
        hypothesis,
        out,
        by = by,
        newdata = newdata,
        mfx = mfx
    )
    out <- hyp$cmp
    tmp <- plan_draws_hypothesis(out, draws, hyp$hyp, hypothesis)
    out <- tmp$out
    draws <- tmp$draws

    plan <- list(
        kind = "predictions",
        n_pred = length(raw_estimate),
        baseline_prediction = raw_estimate,
        linear_predictor = linear_predictor,
        model_matrix_used = model_matrix_used,
        predict_args = list(
            type = type,
            newdata = newdata,
            mfx = mfx,
            dots = dots
        ),
        keep = keep,
        agg = agg$agg,
        hyp = hyp$hyp,
        has_na = anyNA(if (is.null(keep)) raw_estimate else raw_estimate[keep])
    )

    if (!is.null(draws)) {
        # Posterior draws carry their own uncertainty: no delta method, no
        # simulation replay, so the plan is not exported.
        attr(out, "posterior_draws") <- draws
        return(list(cmp = out, plan = NULL))
    }

    baseline <- prediction_plan_apply(plan, raw_estimate)
    validate_plan_replay("prediction", baseline, out[["estimate"]])

    list(cmp = out, plan = plan)
}


prediction_plan_apply_stages <- function(plan, pred) {
    stopifnot(plan_nrow(pred) == plan$n_pred)
    if (!is.null(plan$keep)) {
        pred <- plan_rows(pred, plan$keep)
    }
    apply_plan_stages(pred, plan$agg, plan$hyp)
}


prediction_plan_apply <- function(plan, pred) {
    prediction_plan_apply_stages(plan, pred)$post
}


prediction_plan_predict <- function(.plan, model_perturbed, ...) {
    dots <- sanitize_plan_predict_args(.plan$predict_args$dots, list(...))
    args <- c(
        list(
            model = model_perturbed,
            newdata = .plan$predict_args$newdata,
            type = .plan$predict_args$type,
            mfx = .plan$predict_args$mfx
        ),
        dots
    )
    pred <- do_call(get_predict, args)
    pred[["estimate"]]
}
