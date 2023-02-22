source("helpers.R")
using("marginaleffects")

requiet("robust")

# no validity
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod <- lmRob(mpg ~ hp + cyl, data = mtcars)
expect_slopes(mod, n_unique = 1)
expect_predictions(predictions(mod))




rm(list = ls())