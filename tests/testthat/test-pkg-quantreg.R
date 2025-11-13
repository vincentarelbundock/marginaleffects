testthat::skip_if_not_installed("quantreg")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")
requiet("quantreg")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
mod_simple <- quantreg::rq(mpg ~ wt + am, data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# marginaleffects: rq: Stata
stata <- readRDS(testing_path("stata/stata.rds"))$quantreg_rq_01
model <- suppressWarnings(quantreg::rq(mpg ~ hp * wt + factor(cyl), data = mtcars))
expect_slopes2(model)
mfx <- merge(avg_slopes(model), stata)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001, ignore_attr = TRUE)


# marginaleffects vs. emtrends
stata <- readRDS(testing_path("stata/stata.rds"))$quantreg_rq_01
model <- quantreg::rq(mpg ~ hp * wt + factor(cyl), data = mtcars)
mfx <- slopes(model, variables = "hp", newdata = datagrid(hp = 110, wt = 2, cyl = 4))
em <- suppressMessages(emtrends(model, ~hp, "hp", at = list(hp = 110, wt = 2, cyl = 4)))
em <- tidy(em)
expect_equal(mfx$estimate, em$hp.trend, tolerance = .00001, ignore_attr = TRUE)
expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)


# predictions: rq: no validity
model <- quantreg::rq(mpg ~ hp * wt + factor(cyl), data = mtcars)
pred1 <- predictions(model)
pred2 <- suppressWarnings(predictions(model, newdata = head(mtcars)))
expect_predictions2(model, n_row = nrow(mtcars))
expect_predictions2(model, newdata = head(mtcars), n_row = 6)
expect_equal(pred1$estimate, predict(model), ignore_attr = TRUE)
expect_equal(pred2$estimate, predict(model, newdata = head(mtcars)), ignore_attr = TRUE)


# Issue #829
mod = rq(Sepal.Length ~ Sepal.Width * Petal.Length + Species, tau = .25, data = iris)
cmp = comparisons(mod)
expect_false(any(is.na(cmp$Species)))
expect_false(any(is.na(iris$Species)))
