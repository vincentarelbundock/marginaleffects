source("helpers.R")
using("marginaleffects")

requiet("lme4")

# lme4::lmer
mod <- lme4::lmer(mpg ~ hp + (1 | gear), data = mtcars)
expect_inherits(slopes(mod), "marginaleffects")
expect_warning(slopes(mod, blah = 2), pattern = "Github")


# stats::lm
mod <- lm(mpg ~ hp, data = mtcars)
expect_inherits(slopes(mod), "marginaleffects")
expect_warning(slopes(mod, blah = 2), pattern = "Github")


# deprecation: p_adjust
mod <- lm(mpg ~ factor(cyl), data = mtcars)
expect_error(avg_comparisons(mod, p_adjust = "holm"), pattern = "multcomp")


rm(list = ls())

