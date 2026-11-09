# Apply a `hypothesis` to a table of estimates. A thin wrapper over the
# compiled stage (see `hypothesis_compile()`): the output rows are the stage's
# label template filled with `apply(estimates)`, and posterior draws, when
# present, are pushed through the same `apply()`.
get_hypothesis <- function(
    x,
    hypothesis,
    by = NULL,
    newdata = NULL,
    draws = NULL,
    mfx = NULL) {
    if (is.null(draws)) {
        draws <- attr(x, "posterior_draws")
    }
    compiled <- hypothesis_compile(
        hypothesis,
        x,
        by = by,
        newdata = newdata,
        mfx = mfx,
        draws = draws
    )
    if (is.null(compiled$hyp)) {
        return(x)
    }
    out <- compiled$cmp
    if (!is.null(draws)) {
        out <- hypothesis_frame_draws(compiled$hyp, out, draws)
    }
    out
}
