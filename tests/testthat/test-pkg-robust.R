testthat::skip_if_not_installed("robust")
requiet("robust")

# Basic expectation tests
mod_simple <- robust::lmRob(mpg ~ wt + am, data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# no validity
dat_robust <- mtcars
dat_robust$cyl <- factor(dat_robust$cyl)
mod <- lmRob(mpg ~ hp + cyl, data = mtcars)
expect_slopes2(mod, n_unique = 1)
expect_predictions2(mod)
