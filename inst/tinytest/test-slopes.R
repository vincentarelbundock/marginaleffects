source("helpers.R")

# This should raise an error because we do not know *where* in the predictor
# space the slope should be evaluated
mod <- lm(mpg ~ wt + hp, data = mtcars)
expect_error(
    suppressWarnings(avg_slopes(mod, variables = "hp", newdata = data.frame(wt = 2))),
    "no valid predictor")

s <- avg_slopes(mod, variables = "hp", newdata = datagrid(wt = 2))
expect_inherits(s, "slopes")

# Issue #1538
mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
fun = function(hi, lo, y, x, eps) (hi - lo) / eps * (x / y)
cmp1 <- comparisons(
    mod,
    variables = list("drat" = 0.001),
    eps = 0.001,
    comparison = fun,
    numderiv = list("fdcenter", eps = 1e-5)
)

cmp2 <- comparisons(
    mod,
    variables = "drat",
    eps = 0.001,
    comparison = "eyex",
    numderiv = list("fdcenter", eps = 1e-5)
)

expect_equivalent(cmp1$estimate, cmp2$estimate)
expect_equivalent(cmp1$std.error, cmp2$std.error, tolerance = 1e-4)
