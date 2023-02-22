source("helpers.R")
using("marginaleffects")
requiet("blme")

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/lme4/sleepstudy.csv")
penaltyFn <- function(sigma) dcauchy(sigma, 0, 10, log = TRUE)
fm5 <- blmer(
    Reaction ~ Days + (0 + Days | Subject), data = dat,
    cov.prior = custom(penaltyFn, chol = TRUE, scale = "log"))
fm6 <- blmer(
    Reaction ~ Days + (1 + Days | Subject), 
    data = dat,
    cov.prior = NULL,
    fixef.prior = normal)
mod <- bglmer(vs ~ mpg + (1 | gear), data = mtcars, family = binomial)

expect_slopes(fm5)
expect_slopes(fm6)
expect_slopes(mod)

pre <- predictions(fm5)
expect_predictions(pre)
pre <- predictions(fm6)
expect_predictions(pre)
pre <- predictions(mod)
expect_predictions(pre)



rm(list = ls())