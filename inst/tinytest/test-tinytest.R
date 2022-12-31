
source("helpers.R")
using("marginaleffects")

dat <<- transform(mtcars, gear = factor(gear))
mod <- lm(mpg ~ hp + gear, data = dat)
mfx <- marginaleffects(mod)
pre <- predictions(mod)
mm <- marginalmeans(mod)

expect_marginaleffects(mfx)
expect_marginalmeans(mm)
expect_predictions(pre)

