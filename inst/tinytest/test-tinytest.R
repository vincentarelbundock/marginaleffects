source("helpers.R")
using("marginaleffects")

dat <<- transform(mtcars, gear = factor(gear))
mod <- lm(mpg ~ hp + gear, data = dat)
pre <- predictions(mod)
mm <- marginalmeans(mod)

expect_slopes(mod)
expect_marginalmeans(mm)
expect_predictions(pre)

