source("helpers.R")
using("marginaleffects")

# Drift guard for the two halves of the comparison registry in
# R/sanitize_comparison.R: the forward definitions in
# `comparison_function_dict` and the hand-differentiated gradients in
# `comparison_gradient_exact()`. They are keyed by the same strings, so editing
# one without the other silently leaves a gradient that is correct only for the
# previous definition. The assertions below read the handled keys out of the
# gradient function's own `switch()` rather than restating them, so a key added
# or removed there shows up here.

dict <- marginaleffects:::comparison_function_dict
grad_fun <- marginaleffects:::comparison_gradient_exact
excluded <- marginaleffects:::comparison_gradient_excluded


# Keys handled by the gradient `switch()`, read straight off its call.
find_switch <- function(e) {
    if (!is.call(e)) {
        return(NULL)
    }
    if (identical(e[[1]], as.name("switch"))) {
        return(e)
    }
    for (i in seq_along(e)) {
        got <- tryCatch(find_switch(e[[i]]), error = function(cond) NULL)
        if (!is.null(got)) {
            return(got)
        }
    }
    NULL
}
sw <- find_switch(body(grad_fun))
expect_false(is.null(sw), info = "the gradient switch() was located")
handled <- names(sw)
handled <- handled[nzchar(handled)]
expect_true(length(handled) > 0L, info = "the gradient switch() has named branches")


# 1. Nothing is differentiated that the forward dictionary does not define.
expect_equal(
    sort(setdiff(handled, names(dict))),
    character(0),
    info = "every gradient key exists in comparison_function_dict"
)

# 2. Every forward shorthand either has a closed-form gradient or is excluded
#    by design. `eyex` / `eydx` and their averaged variants divide by the
#    observed outcome `y`; that marks their groups `uses_y`, which disqualifies
#    the analytic path upstream, so they are never offered a gradient.
expect_equal(
    sort(setdiff(names(dict), c(handled, excluded))),
    character(0),
    info = "every forward shorthand is differentiated or deliberately excluded"
)
expect_equal(
    sort(intersect(handled, excluded)),
    character(0),
    info = "no key is both differentiated and excluded"
)
expect_equal(
    sort(setdiff(excluded, names(dict))),
    character(0),
    info = "the exclusion list names only real shorthands"
)

# 3. The exclusions really do return NULL, so the analytic path cannot pick
#    them up by accident if the upstream `uses_y` gate ever changes.
for (key in excluded) {
    expect_null(
        grad_fun(key, hi = c(0.4, 0.6), lo = c(0.3, 0.5), args = list(eps = 0.1, x = c(1, 2), w = c(1, 2))),
        info = paste(key, "is not differentiated")
    )
}
expect_null(
    grad_fun("not_a_comparison", hi = 0.4, lo = 0.3, args = list()),
    info = "an unknown key returns NULL"
)


# 4. Numerical spot-check: each recorded gradient must match a central finite
#    difference of the forward function it mirrors, at one fixed point. `hi`
#    and `lo` stay strictly inside (0, 1) because lnor and lnratio need it, and
#    the weights are non-constant so the `*avgwts` branches differ from the
#    plain averages.
set.seed(1024)
n <- 5L
point <- list(
    hi = runif(n, 0.2, 0.8),
    lo = runif(n, 0.2, 0.8),
    eps = 0.037,
    x = runif(n, 1, 3),
    w = runif(n, 0.5, 2)
)
step <- 1e-6

# Forward function evaluated with whatever formals it declares, all drawn from
# the fixed point above.
evaluate <- function(fun, hi, lo) {
    args <- point
    args$hi <- hi
    args$lo <- lo
    do.call(fun, args[names(formals(fun))])
}

# d(fun)/d(hi[i]) or d(fun)/d(lo[i]). A row-wise shorthand returns a vector
# whose i-th element is the only one that moves; an averaging shorthand returns
# a scalar.
finite_difference <- function(fun, side, i) {
    up <- down <- point[[side]]
    up[i] <- up[i] + step
    down[i] <- down[i] - step
    hi_up <- if (side == "hi") up else point$hi
    lo_up <- if (side == "hi") point$lo else up
    hi_down <- if (side == "hi") down else point$hi
    lo_down <- if (side == "hi") point$lo else down
    delta <- (evaluate(fun, hi_up, lo_up) - evaluate(fun, hi_down, lo_down)) / (2 * step)
    if (length(delta) == 1L) delta else delta[i]
}

for (key in handled) {
    fun <- dict[[key]]
    grad <- grad_fun(key, hi = point$hi, lo = point$lo, args = point)
    expect_false(is.null(grad), info = paste(key, "returns a gradient"))
    expect_equal(length(grad$hi), n, info = paste(key, "hi gradient length"))
    expect_equal(length(grad$lo), n, info = paste(key, "lo gradient length"))
    for (side in c("hi", "lo")) {
        reference <- vapply(seq_len(n), function(i) finite_difference(fun, side, i), numeric(1))
        scale <- max(1, max(abs(reference)))
        expect_true(
            max(abs(grad[[side]] - reference)) < 1e-5 * scale,
            info = paste(key, side, "gradient matches central finite difference")
        )
    }
}
