source("helpers.R", local = TRUE)

# factor in formula
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
expect_warning(marginaleffects(mod))
