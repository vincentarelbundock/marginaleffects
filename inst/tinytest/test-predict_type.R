source("helpers.R")
using("marginaleffects")


# sanity gives informative error for all the functions
dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat <- dat
mod <- lm(mpg ~ hp + cyl, data = dat)
expect_error(comparisons(mod, type = "junk"), pattern = "Must be element")
expect_error(predictions(mod, type = "junk"), pattern = "Must be element")
expect_error(slopes(mod, type = "junk"), pattern = "Must be element")
expect_error(marginal_means(mod, type = "junk"), pattern = "Must be element")



# error: multivariate
requiet("pscl")
dat2 <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/pscl/bioChemists.csv")
model <- hurdle(art ~ phd + fem | ment, data = dat2, dist = "negbin")
mfx <- slopes(model, type = "prob")
expect_true(all(as.character(0:19) %in% mfx$group))


rm(list = ls())