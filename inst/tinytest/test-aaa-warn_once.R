source("helpers.R")
using("marginaleffects")

# factor in formula
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
expect_warning(slopes(mod))
