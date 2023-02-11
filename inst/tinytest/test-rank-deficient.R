source("helpers.R")
using("marginaleffects")

# rank deficient
dat <- mtcars
dat$gear <- as.factor(dat$gear)
dat$cyl <- as.factor(dat$cyl)
dat <- dat
m <- glm(am ~ gear * cyl, data = dat, family = binomial())
expect_warning(comparisons(m), pattern = "rank deficient")
expect_inherits(suppressWarnings(comparisons(m)), "comparisons")



rm(list = ls())