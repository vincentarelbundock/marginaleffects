skip("incomplete tests just for demo")
skip_if_not_installed("cmdstanr")
library(brms)
library(insight)
library(marginaleffects)

m1 <- brm(mpg ~ hp, data = mtcars, backend = "cmdstanr")
m2 <- brm(mpg ~ hp + drat, data = mtcars, backend = "cmdstanr")
m3 <- brm(mpg ~ hp + drat + mo(cyl), data = mtcars, backend = "cmdstanr")

pp_average(m1,
           m2 = m2,
           m3 = m3,
           newdata = head(mtcars))

predictions(m1,
            m2 = m2,
            m3 = m3,
            type = "average",
            newdata = head(mtcars))

cmp <- comparisons(m1,
            m2 = m2,
            m3 = m3,
            type = "average",
            newdata = head(mtcars))
cmp


# Manual computation: contrast of 20 about the observed values
set.seed(1024)
dat_lo <- dat_hi <- head(mtcars)
dat_hi$hp <- dat_hi$hp + 10
dat_lo$hp <- dat_lo$hp - 10
avg1 = pp_average(
    m1,
    m2 = m2,
    m3 = m3,
    summary = FALSE,
    newdata = dat_lo)
avg2 = pp_average(
    m1,
    m2 = m2,
    m3 = m3,
    summary = FALSE,
    newdata = dat_hi)
contr <- avg2 - avg1
t(apply(contr, 2, quantile, c(.5, .025, .975)))

# Automatic = Manual
set.seed(1024)
comparisons(m1,
            m2 = m2,
            m3 = m3,
            type = "average",
            contrast_numeric = 20,
            newdata = head(mtcars))
