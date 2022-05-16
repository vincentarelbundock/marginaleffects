source("helpers.R")

# lme4::lmer
requiet("lme4")
mod <- lmer(mpg ~ hp + (1 | gear), data = mtcars)
expect_inherits(marginaleffects(mod), "marginaleffects")
expect_warning(marginaleffects(mod, blah = 2), pattern = "Valid.*Github")



# stats::lm
mod <- lm(mpg ~ hp, data = mtcars)
expect_inherits(marginaleffects(mod), "marginaleffects")
expect_warning(marginaleffects(mod, blah = 2), pattern = "Github")


