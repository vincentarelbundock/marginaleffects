source("helpers.R")
using("marginaleffects")

exit_if_not(requiet("lme4"))

# lme4::lmer
mod <- lmer(mpg ~ hp + (1 | gear), data = mtcars)
expect_inherits(slopes(mod), "marginaleffects")
expect_warning(slopes(mod, blah = 2), pattern = "Github")


# stats::lm
mod <- lm(mpg ~ hp, data = mtcars)
expect_inherits(slopes(mod), "marginaleffects")
expect_warning(slopes(mod, blah = 2), pattern = "Github")


