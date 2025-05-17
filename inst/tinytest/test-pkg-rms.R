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
mfx <- slopes(model, newdata = data.frame(mpg = 30), type = "lp", eps = 1 / 1000 * diff(range(mtcars$mpg)))
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


# Issue #1428
requiet("tibble")
data <- tibble::tibble(
    y = rbinom(100, 1, .4),
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rep(c("A", "B"), 50)
)
f <- lrm(y ~ ., data = data)
p <- suppressWarnings(get_predict(f))
expect_inherits(p, "data.frame")
expect_equal(dim(p), c(100, 2))
expect_warning(get_predict(f), pattern = "Converting.*tibble")


rm(list = ls())
