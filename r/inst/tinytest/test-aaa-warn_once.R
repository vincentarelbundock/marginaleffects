source("helpers.R")

op <- options(
    marginaleffects_safe = TRUE,
    marginaleffects_warning_factor_on_the_fly_conversion = TRUE,
    marginaleffects_invlink_link_hypothesis_must_be_on_link_scale = TRUE
)

# factor in formula
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
expect_warning(slopes(mod))


# Issue #1447: invlink(link) hypothesis scale
dat <- transform(mtcars, cyl = factor(cyl))
mod <- glm(am ~ hp + cyl, data = dat, family = binomial)
expect_warning(predictions(mod, hypothesis = 3), pattern = "invlink")

options(op)
