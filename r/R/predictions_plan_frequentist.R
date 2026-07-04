prediction_plan_build_frequentist <- function(
    out,
    raw_estimate,
    keep,
    newdata,
    type,
    mfx,
    dots,
    by = NULL,
    hypothesis = NULL,
    verbose = TRUE,
    ...) {
    agg <- estimate_plan_record_agg(
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
        n_pred = length(raw_estimate),
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
    estimate_plan_check_baseline("prediction", baseline, out[["estimate"]])

    list(cmp = out, plan = plan)
}


prediction_plan_apply <- function(plan, pred) {
    stopifnot(length(pred) == plan$n_pred)
    if (!is.null(plan$keep)) {
        pred <- pred[plan$keep]
    }
    estimate_plan_apply_post(pred, plan$agg, plan$hyp)
}


prediction_plan_predict <- function(.plan, model_perturbed, ...) {
    dots <- estimate_plan_predict_dots(.plan$predict_args$dots, list(...))
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
