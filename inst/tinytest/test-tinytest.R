source("helpers.R")
using("marginaleffects")

dat <- transform(mtcars, gear = factor(gear))
mod <- lm(mpg ~ hp + gear, data = dat)
pre <- predictions(mod)
mm <- marginal_means(mod)

expect_slopes(mod)
expect_marginal_means(mm)
expect_predictions(pre)


rm(list = ls())