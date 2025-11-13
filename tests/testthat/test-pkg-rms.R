testthat::skip_if_not_installed("polspline")
testthat::skip_if_not_installed("rms")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")
requiet("polspline")
requiet("rms")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
mod_simple <- lrm(am ~ mpg + wt, data = mtcars)
expect_slopes2(mod_simple, newdata = mtcars)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# lmr: marginaleffects vs emtrends
model <- rms::lrm(am ~ mpg, mtcars)
expect_slopes2(model, type = "lp", n_unique = 1)
mfx <- slopes(model, newdata = data.frame(mpg = 30), type = "lp", eps = 1 / 1000 * diff(range(mtcars$mpg)))
em <- emtrends(model, ~mpg, "mpg", at = list(mpg = 30))
em <- tidy(em)
expect_equal(mfx$estimate, em$mpg.trend, ignore_attr = TRUE)
expect_equal(mfx$std.error, em$std.error, tolerance = .0001, ignore_attr = TRUE)


# predictions: rms: no validity
model <- rms::lrm(am ~ mpg, mtcars)
pred1 <- predictions(model, type = "lp")
pred2 <- predictions(model, type = "lp", newdata = head(mtcars))
expect_predictions2(model, n_row = 32)
expect_predictions2(model, n_row = 6)


# comparisons
mod <- ols(mpg ~ hp, mtcars)
c1 <- comparisons(mod, type = "lp")
expect_s3_class(c1, "comparisons")

mod <- lrm(am ~ hp, mtcars)
c1 <- comparisons(mod, type = "fitted")
c2 <- comparisons(mod, type = "lp")
expect_s3_class(c1, "comparisons")
expect_s3_class(c2, "comparisons")



mod <- orm(cyl ~ hp, mtcars)
c1 <- comparisons(mod, type = "fitted")
c2 <- comparisons(mod, type = "lp")
c3 <- comparisons(mod, type = "mean")
expect_s3_class(c1, "comparisons")
expect_s3_class(c2, "comparisons")

expect_error(comparisons(mod, vcov = "HC3"), regexp = "supported")


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
expect_s3_class(p, "data.frame")
expect_equal(dim(p), c(100, 1))
expect_warning(get_predict(f), regexp = "Converting.*tibble")


mod <- lrm(cyl ~ hp, mtcars)
c1 <- comparisons(mod, type = "fitted")
c2 <- comparisons(mod, type = "lp")
expect_s3_class(c1, "comparisons")
expect_s3_class(c2, "comparisons")


# Issue 1600
dat_rms <- mtcars
dat_rms$gear_ord <- factor(dat_rms$gear, ordered = TRUE)
mod <- orm(mpg ~ gear_ord + hp * wt, data = dat_rms, x = TRUE, y = TRUE)
expect_warning(
    avg_comparisons(mod, variables = "gear_ord", type = "lp"),
    regexp = "Ordered factors sometimes cause issues with `rms` models")
