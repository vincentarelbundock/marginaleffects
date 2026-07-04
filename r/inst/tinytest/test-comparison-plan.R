source("helpers.R")
using("marginaleffects")

mod <- lm(mpg ~ hp * wt + factor(cyl), data = mtcars)

cmp <- avg_comparisons(
    mod,
    variables = "hp",
    comparison = function(hi, lo) mean(hi - lo)
)
expect_equal(nrow(cmp), 1)
expect_true("std.error" %in% colnames(cmp))
expect_false(anyNA(cmp$std.error))

cmp <- comparisons(
    mod,
    variables = "hp",
    comparison = function(hi, lo, x) (hi - lo) * x
)
expect_equal(nrow(cmp), nrow(mtcars))
expect_true("std.error" %in% colnames(cmp))

expect_error(
    comparisons(mod, byfun = sum),
    pattern = "`byfun`.*not supported.*`hypothesis`"
)
expect_error(
    avg_comparisons(mod, byfun = sum),
    pattern = "`byfun`.*not supported.*`hypothesis`"
)
expect_error(
    slopes(mod, byfun = sum),
    pattern = "`byfun`.*not supported.*`hypothesis`"
)
expect_error(
    avg_slopes(mod, byfun = sum),
    pattern = "`byfun`.*not supported.*`hypothesis`"
)

cmp_string <- comparisons(
    mod,
    variables = c("hp", "wt"),
    newdata = "mean",
    hypothesis = "b1 = b2"
)
cmp_matrix <- comparisons(
    mod,
    variables = c("hp", "wt"),
    newdata = "mean",
    hypothesis = c(1, -1)
)
expect_equivalent(cmp_string$estimate, cmp_matrix$estimate)
expect_equivalent(cmp_string$std.error, cmp_matrix$std.error)

cmp_string <- comparisons(
    mod,
    variables = c("hp", "wt"),
    newdata = "mean",
    hypothesis = "plogis(b1) - plogis(b2) = 0"
)
expect_equal(nrow(cmp_string), 1)
expect_true("std.error" %in% colnames(cmp_string))

cmp_formula <- comparisons(
    mod,
    variables = c("hp", "wt"),
    newdata = "mean",
    hypothesis = ~pairwise
)
expect_equal(nrow(cmp_formula), 1)
expect_true("std.error" %in% colnames(cmp_formula))

ok <- try(
    marginaleffects:::validate_plan_replay(
        "comparison",
        c(1, 2, 3),
        c(1, 2 + 1e-10, 3)
    ),
    silent = TRUE
)
expect_false(inherits(ok, "try-error"))

expect_error(
    marginaleffects:::validate_plan_replay(
        "comparison",
        c(1, 2, 3),
        c(1, 2.01, 3)
    ),
    pattern = "comparison plan baseline check failed"
)
