source("helpers.R")

# TODO: intentional breakage to test github actions
tinytest::expect_true(FALSE)

# factor in formula
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
expect_warning(marginaleffects(mod))

