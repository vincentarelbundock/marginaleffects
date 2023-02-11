source("helpers.R")
using("marginaleffects")


# marginaleffects: logical
dat <- mtcars
dat$am <- as.logical(dat$am)
mod <- glm(vs ~ am + mpg, data = dat, family = binomial)
mfx <- slopes(mod)
expect_inherits(mfx, "marginaleffects")
expect_equivalent(nrow(mfx), nrow(dat) * 2)


rm(list = ls())