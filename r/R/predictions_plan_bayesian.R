prediction_plan_build_bayesian <- function(
    out,
    draws,
    newdata,
    mfx,
    by = NULL,
    hypothesis = NULL,
    verbose = TRUE,
    ...) {
    out <- get_by(
        out,
        draws = draws,
        newdata = newdata,
        by = by,
        verbose = verbose,
        ...
    )
    draws <- attr(out, "posterior_draws")
    out <- get_hypothesis(
        out,
        hypothesis = hypothesis,
        by = by,
        newdata = newdata,
        draws = draws,
        mfx = mfx
    )

    list(cmp = out, plan = NULL)
}
