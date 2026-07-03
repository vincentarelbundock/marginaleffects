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
