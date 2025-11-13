# newdata must be explicit otherwise this only works interactively
testthat::skip_if_not_installed("AER")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")

requiet("AER")
requiet("emmeans")
requiet("broom")
tol_se <- 1e-4

# Basic expectation tests
mod_simple <- AER::tobit(mpg ~ wt + am, data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

dat <<- get_dataset("Affairs", "AER")


# tobit: marginaleffects vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))$aer_tobit
mod1 <- tobit(
    affairs ~ age + yearsmarried + religiousness + occupation + rating,
    data = dat
)
mfx <- merge(tidy(slopes(mod1, newdata = dat)), stata)
expect_slopes2(mod1, n_unique = 1, newdata = dat)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .00001, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = 1e-4, ignore_attr = TRUE)

stata <- readRDS(testing_path("stata/stata.rds"))$aer_tobit_right
mod2 <- tobit(
    affairs ~ age + yearsmarried + religiousness + occupation + rating,
    right = 4,
    data = dat
)
mfx <- merge(tidy(slopes(mod2, newdata = dat)), stata)
expect_slopes2(mod2, n_unique = 1, newdata = dat)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .1, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .1, ignore_attr = TRUE)


# marginaleffects vs. emtrends
mod <- tobit(affairs ~ age + yearsmarried, data = dat)
mfx <- slopes(mod, newdata = datagrid(age = 30, yearsmarried = 5))
em1 <- emmeans::emtrends(mod, ~age, "age", at = list(age = 30, yearsmarried = 5))
em2 <- emmeans::emtrends(mod, ~yearsmarried, "yearsmarried", at = list(age = 30, yearsmarried = 5))
em1 <- tidy(em1)
em2 <- tidy(em2)
expect_equal(mfx$estimate[1], em1$age.trend, ignore_attr = TRUE)
expect_equal(mfx$std.error[1], em1$std.error, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx$estimate[2], em2$yearsmarried.trend, ignore_attr = TRUE)
expect_equal(mfx$std.error[2], em2$std.error, tolerance = .0002, ignore_attr = TRUE)


# predictions: tobit: no validity
mod <- AER::tobit(
    affairs ~ age + yearsmarried + religiousness + occupation + rating,
    data = dat
)
pred <- predictions(mod, newdata = dat)
expect_predictions2(mod, newdata = dat, n_row = nrow(dat))
