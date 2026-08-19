# Shared comparison helpers for the Stata golden-fixture files
# (test-stata-models.R, test-stata-legacy.R, test-stata-misc.R,
# test-stata-args.R).
#
# Source this with `local = TRUE`, after assigning `golden` and `tested`:
#
#     golden <- read.csv(golden_path, stringsAsFactors = FALSE)
#     tested <- character()
#     source("stata_helpers.R", local = TRUE)
#
# `local = TRUE` matters twice over. It puts these functions in the test
# file's own environment, so `golden` and `tested <<-` resolve there rather
# than in the global environment; and it lets the `expect_*` calls below
# resolve to the recording versions tinytest binds for the file under test.
# Sourced globally instead, every nested expectation would evaluate against
# the plain package functions and go uncounted.

# all.equal() silently falls back to an *absolute* comparison when the mean
# magnitude of the target drops below `tolerance`, which would let a nominally
# loose bound accept an unbounded relative error on small standard errors.
# Passing `scale` keeps every comparison relative.
expect_rel <- function(current, target, tolerance, info) {
    scale <- max(mean(abs(target)), .Machine$double.eps)
    expect_equal(current, target, tolerance = tolerance, scale = scale, info = info)
}

# Compare a marginaleffects result against the rows one fixture contributes to
# the golden CSV, and record the fixture as covered.
#
# Tolerances are opt-in: a column is compared only where its tolerance is not
# NA. Stata exports the whole of r(table), but the two packages do not always
# use the same reference distribution, so the interval bounds in particular are
# only checked where that agreement is expected -- `regress` margins are t-based
# on df_r while marginaleffects defaults to z, and for a glm the default
# type = "invlink(link)" builds the interval on the link scale and back
# transforms it rather than adding a symmetric delta-method margin.
#
# The two packages do not always order rows the same way either. Reorder
# whichever side is out of step:
#   `order_x`      positional index into the marginaleffects result
#   `terms`        reorder the result so `term` follows Stata's dydx() order
#   `order_golden` positional index into the golden rows
check_stata <- function(
    fixture,
    x,
    estimate = NA_real_,
    se = NA_real_,
    ci = NA_real_,
    statistic = NA_real_,
    order_x = NULL,
    terms = NULL,
    order_golden = NULL
) {
    s <- golden[golden$fixture == fixture, , drop = FALSE]
    if (!is.null(order_golden)) s <- s[order_golden, , drop = FALSE]
    tested <<- union(tested, fixture)
    x <- as.data.frame(x)
    if (!is.null(order_x)) x <- x[order_x, , drop = FALSE]
    if (!is.null(terms)) x <- x[match(terms, x$term), , drop = FALSE]
    expect_equal(nrow(x), nrow(s), info = paste(fixture, "nrow"))
    if (nrow(x) != nrow(s)) return(invisible(NULL))
    if (!is.na(estimate)) {
        expect_rel(x$estimate, s$estimate, estimate, paste(fixture, "estimate"))
    }
    if (!is.na(se)) {
        expect_rel(x$std.error, s$std_error, se, paste(fixture, "std.error"))
    }
    if (!is.na(ci)) {
        expect_rel(x$conf.low, s$conf_low, ci, paste(fixture, "conf.low"))
        expect_rel(x$conf.high, s$conf_high, ci, paste(fixture, "conf.high"))
    }
    if (!is.na(statistic)) {
        expect_rel(x$statistic, s$statistic, statistic, paste(fixture, "statistic"))
    }
    invisible(NULL)
}
