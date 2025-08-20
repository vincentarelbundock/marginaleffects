source("helpers.R")

# Issue #1538
mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
cmp1 <- comparisons(
    mod,
    variables = list("hp" = 0.001),
    eps = 0.001,
    comparison = function(hi, lo, y, x, eps) (hi - lo) / eps * (x / y),
    numderiv = list("fdcenter", eps = 1e-5)
)

cmp2 <- comparisons(
    mod,
    variables = "hp",
    eps = 0.001,
    comparison = "eyex",
    numderiv = list("fdcenter", eps = 1e-5)
)

expect_equivalent(cmp1$estimate, cmp2$estimate)
expect_equivalent(cmp1$std.error, cmp2$std.error, tolerance = 1e-4)
