source("helpers.R")
using("marginaleffects")

requiet("polspline")
requiet("rms")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
mod_simple <- lrm(am ~ mpg + wt, data = mtcars)
expect_slopes(mod_simple, newdata = mtcars)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

# lmr: marginaleffects vs emtrends
model <- rms::lrm(am ~ mpg, mtcars)
expect_slopes(model, type = "lp", n_unique = 1)
mfx <- slopes(model, newdata = data.frame(mpg = 30), type = "lp", eps = 1 / 1000 * diff(range(mtcars$mpg)))
em <- emtrends(model, ~mpg, "mpg", at = list(mpg = 30))
em <- tidy(em)
expect_equivalent(mfx$estimate, em$mpg.trend)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .0001)


# predictions: rms: no validity
model <- rms::lrm(am ~ mpg, mtcars)
pred1 <- predictions(model, type = "lp")
pred2 <- predictions(model, type = "lp", newdata = head(mtcars))
expect_predictions(model, n_row = 32)
expect_predictions(model, n_row = 6)


# comparisons
mod <- ols(mpg ~ hp, mtcars)
c1 <- comparisons(mod, type = "lp")
expect_inherits(c1, "comparisons")

mod <- lrm(am ~ hp, mtcars)
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
expect_equal(dim(p), c(100, 1))
expect_warning(get_predict(f), pattern = "Converting.*tibble")


mod <- lrm(cyl ~ hp, mtcars)
c1 <- comparisons(mod, type = "fitted")
c2 <- comparisons(mod, type = "lp")
expect_inherits(c1, "comparisons")
expect_inherits(c2, "comparisons")


# Issue 1600
dat <- mtcars
dat$gear_ord <- factor(dat$gear, ordered = TRUE)
mod <- orm(mpg ~ gear_ord + hp * wt, data = dat, x = TRUE, y = TRUE)
expect_warning(
    avg_comparisons(mod, variables = "gear_ord", type = "lp"),
    pattern = "Ordered factors sometimes cause issues with `rms` models")


# autodiff
if (!AUTODIFF) exit_file("autodiff")
mod <- ols(mpg ~ hp, mtcars)
autodiff(TRUE)
expect_message(cmp1 <- comparisons(mod))
autodiff(FALSE)
cmp2 <- comparisons(mod)
expect_equivalent(cmp1$estimate, cmp2$estimate)
expect_equivalent(cmp1$std.error, cmp2$std.error, tol = 1e-5)

mod <- lrm(am ~ hp * wt, mtcars)
autodiff(TRUE)
expect_message(cmp1 <- comparisons(mod))
autodiff(FALSE)
cmp2 <- comparisons(mod)
expect_equivalent(cmp1$estimate, cmp2$estimate)
expect_equivalent(cmp1$std.error, cmp2$std.error, tol = 1e-4)
