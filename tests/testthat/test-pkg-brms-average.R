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

# not sure why the intervals are so large
draws <- attr(cmp, "posterior_draws")
t(apply(draws, 1, quantile, c(.5, .025, .975)))
