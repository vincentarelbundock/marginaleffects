source("helpers.R")
using("marginaleffects")

requiet("robust")

# Basic expectation tests
mod_simple <- robust::lmRob(mpg ~ wt + am, data = mtcars)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

# no validity
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod <- lmRob(mpg ~ hp + cyl, data = mtcars)
expect_slopes(mod, n_unique = 1)
expect_predictions(mod)
