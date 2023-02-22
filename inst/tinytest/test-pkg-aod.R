source("helpers.R")
using("marginaleffects")

requiet("aod")

# betabin: no validity
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/aod/orob2.csv")

# character variables should be padded, but I am lazy
mod <- betabin(cbind(y, n - y) ~ seed, ~ 1, data = dat)
expect_error(slopes(mod), pattern = "support.*character")

# factor variables work
dat$seed <- factor(dat$seed)
mod <- betabin(cbind(y, n - y) ~ seed, ~ 1, data = dat)
expect_slopes(mod, n_unique = 1)

pre <- predictions(mod)
expect_predictions(pre)



rm(list = ls())