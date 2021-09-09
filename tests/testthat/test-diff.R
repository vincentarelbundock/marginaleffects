library(marginaleffects)
library(insight)

dat <- mtcars
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ hp + am + factor(cyl), data = dat)
mfx <- marginaleffects(mod)
