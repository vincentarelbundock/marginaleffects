source("helpers.R")
using("marginaleffects")

requiet("scam")

# no validity
set.seed(4)
n <- 200
x1 <- runif(n) * 6 - 3
f1 <- 3 * exp(-x1^2) # unconstrained term
x2 <- runif(n) * 4 - 1;
f2 <- exp(4 * x2) / (1 + exp(4 * x2)) # monotone increasing smooth
y <- f1 + f2 + rnorm(n) * .5
dat <- data.frame(x1 = x1, x2 = x2, y = y)
mod <- scam(y ~ s(x1, bs = "cr") + s(x2, bs = "mpi"), data = dat)
suppressWarnings(expect_slopes(mod))
expect_predictions(predictions(mod))



rm(list = ls())