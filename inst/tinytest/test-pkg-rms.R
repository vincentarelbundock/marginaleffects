source("helpers.R")
using("marginaleffects")

requiet("polspline")
requiet("rms")
requiet("emmeans")
requiet("broom")

# lmr: marginaleffects vs emtrends
model <- rms::lrm(am ~ mpg, mtcars)
void <- capture.output({
    expect_slopes(model, type = "lp", n_unique = 1)
})
mfx <- slopes(model, newdata = data.frame(mpg = 30), type = "lp")
em <- emtrends(model, ~mpg, "mpg", at = list(mpg = 30))
em <- tidy(em)
expect_equivalent(mfx$estimate, em$mpg.trend)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .0001)


# predictions: rms: no validity
model <- rms::lrm(am ~ mpg, mtcars)
pred1 <- predictions(model, type = "lp")
pred2 <- predictions(model, type = "lp", newdata = head(mtcars))
expect_predictions(pred1, n_row = 32)
expect_predictions(pred2, n_row = 6)


# comparisons
mod <- ols(mpg ~ hp, mtcars)
c1 <- comparisons(mod, type = "lp")
expect_inherits(c1, "comparisons")

mod <- lrm(am ~ hp, mtcars)
c1 <- comparisons(mod, type = "fitted")
c2 <- comparisons(mod, type = "lp")
expect_inherits(c1, "comparisons")
expect_inherits(c2, "comparisons")

mod <- lrm(cyl ~ hp, mtcars)
c1 <- comparisons(mod, type = "fitted")
c2 <- comparisons(mod, type = "lp")
expect_inherits(c1, "comparisons")
expect_inherits(c2, "comparisons")

mod <- orm(cyl ~ hp, mtcars)
c1 <- comparisons(mod, type = "fitted")
c2 <- comparisons(mod, type = "lp")
c3 <- comparisons(mod, type = "mean")
expect_inherits(c1, "comparisons")
expect_inherits(c2, "comparisons")

expect_error(comparisons(mod, vcov = "HC3"), pattern = "supported") 



rm(list = ls())