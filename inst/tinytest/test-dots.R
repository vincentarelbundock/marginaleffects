source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("lme4")

# lme4::lmer
mod <- lmer(mpg ~ hp + (1 | gear), data = mtcars)
expect_inherits(marginaleffects(mod), "marginaleffects")
expect_warning(marginaleffects(mod, blah = 2), pattern = "Github")


# stats::lm
mod <- lm(mpg ~ hp, data = mtcars)
expect_inherits(marginaleffects(mod), "marginaleffects")
expect_warning(marginaleffects(mod, blah = 2), pattern = "Github")


