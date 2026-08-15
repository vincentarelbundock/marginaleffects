prediction_plan_build_frequentist <- function(
    out,
    raw_estimate,
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
    agg <- record_plan_aggregation(
        out,
        newdata = newdata,
        by = by,
        verbose = verbose,
        ...
    )
    out <- agg$out
    hyp <- hypothesis_compile(
        hypothesis,
        out,
        by = by,
        newdata = newdata,
        mfx = mfx
    )
    out <- hyp$cmp
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
    baseline <- prediction_plan_apply(plan, raw_estimate)
    validate_plan_replay("prediction", baseline, out[["estimate"]])

    list(cmp = out, plan = plan)
}


prediction_plan_apply <- function(plan, pred) {
    stopifnot(length(pred) == plan$n_pred)
    if (!is.null(plan$keep)) {
        pred <- pred[plan$keep]
    }
    apply_plan_aggregation_and_hypothesis(pred, plan$agg, plan$hyp)
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
