source("helpers.R")
requiet("marginaleffects")

expect_false(TRUE) # intentional breakage to test CI

# factor in formula
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
expect_warning(slopes(mod))



rm(list = ls())
