get_predictions <- function(
    mfx,
    type,
    model_perturbed = NULL, # important for perturbed model
    by = NULL,
    hypothesis = NULL,
    verbose = TRUE,
    hi = NULL, # sink hole for shared comparisons/predictions call
    lo = NULL, # sink hole avoids pushing these variables through ... in get_predict()
    original = NULL, # sink hole
    ...) {
    prediction_plan_build(
        mfx = mfx,
        type = type,
        model_perturbed = model_perturbed,
        by = by,
        hypothesis = hypothesis,
        verbose = verbose,
        hi = hi,
        lo = lo,
        original = original,
        ...
    )$cmp
}
