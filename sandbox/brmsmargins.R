library(brms)
library(brmsmargins)
library(marginaleffects)
library(data.table)
h <- 5e-5

# Not supported by `marginaleffects`

* `brmsmargins::marginalcoef()`

# Average Marginal Effects

bayes.logistic <- brm(
  vs ~ am + mpg, data = mtcars,
  family = "bernoulli", seed = 1234,
  silent = 2, refresh = 0,
  chains = 4L, cores = 4L, backend = "cmdstanr")

brmsmargins(
  bayes.logistic,
  add = data.frame(mpg = c(0, 0 + h)),
  contrasts = cbind("AME MPG" = c(-1 / h, 1 / h)),
  CI = 0.95, CIType = "HDI")$ContrastSummary

mfx <- marginaleffects(bayes.logistic) 
summary(mfx)

aggregate(draw ~ term, data = posteriordraws(mfx), FUN = mean)

# Mixed Effects Logistic Regression

d <- withr::with_seed(
  seed = 12345, code = {
    nGroups <- 100
    nObs <- 20
    theta.location <- matrix(rnorm(nGroups * 2), nrow = nGroups, ncol = 2)
    theta.location[, 1] <- theta.location[, 1] - mean(theta.location[, 1])
    theta.location[, 2] <- theta.location[, 2] - mean(theta.location[, 2])
    theta.location[, 1] <- theta.location[, 1] / sd(theta.location[, 1])
    theta.location[, 2] <- theta.location[, 2] / sd(theta.location[, 2])
    theta.location <- theta.location %*% chol(matrix(c(1.5, -.25, -.25, .5^2), 2))
    theta.location[, 1] <- theta.location[, 1] - 2.5
    theta.location[, 2] <- theta.location[, 2] + 1
    d <- data.table(
      x = rep(rep(0:1, each = nObs / 2), times = nGroups))
    d[, ID := rep(seq_len(nGroups), each = nObs)]

    for (i in seq_len(nGroups)) {
      d[ID == i, y := rbinom(
        n = nObs,
        size = 1,
        prob = plogis(theta.location[i, 1] + theta.location[i, 2] * x))
        ]
    }
    copy(d)
  })

mlogit <- brms::brm(
  y ~ 1 + x + (1 + x | ID), family = "bernoulli",
  data = d, seed = 1234,
  silent = 2, refresh = 0,
  chains = 4L, cores = 4L, backend = "cmdstanr")

## AMEs: Default

bm <- brmsmargins(
  mlogit,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  k = 100L, seed = 1234)
data.frame(bm$ContrastSummary)

mfx <- marginaleffects(mlogit, re_formula = NA) 
summary(mfx)

aggregate(draw ~ term, data = posteriordraws(mfx), FUN = mean)

## AMEs: Integrate Out Random Effects

bm <- brmsmargins(
  mlogit,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  effects = "integrateoutRE",
  k = 100L, seed = 1234)
data.frame(bm$ContrastSummary)

mfx <- marginaleffects(mlogit) 
summary(mfx)

aggregate(draw ~ term, data = posteriordraws(mfx), FUN = mean)

mc.logit <- marginalcoef(mlogit, CI = .95)

# AMEs for Fixed Effects Location Scale Models

d <- withr::with_seed(
  seed = 12345, code = {
    nObs <- 1000L
    d <- data.table(
      grp = rep(0:1, each = nObs / 2L),
      x = rnorm(nObs, mean = 0, sd = 0.25))
    d[, y := rnorm(nObs,
                   mean = x + grp,
                   sd = exp(1 + x + grp))]
    copy(d)
  })

ls.fe <- brm(bf(
  y ~ 1 + x + grp,
  sigma ~ 1 + x + grp),
  family = "gaussian",
  data = d, seed = 1234,
  silent = 2, refresh = 0,
  chains = 4L, cores = 4L, backend = "cmdstanr")

## Fixed effects only

bm <- brmsmargins(
  ls.fe,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  CI = 0.95, CIType = "ETI",
  effects = "fixedonly")
data.frame(bm$ContrastSummary)

mfx <- marginaleffects(ls.fe, re_formula = NA)
aggregate(draw ~ term, data = posteriordraws(mfx), FUN = mean)

## Discrete change (contrast) -- Not sure

bm <- brmsmargins(
  ls.fe,
  at = data.frame(grp = c(0, 1)),
  contrasts = cbind("AME grp" = c(-1, 1)),
  CI = 0.95, CIType = "ETI", dpar = "sigma",
  effects = "fixedonly")
data.frame(bm$ContrastSummary)

mfx <- comparisons(
  ls.fe,
  variables = "grp",
  re_formula = NA,
  contrast_numeric = c(0, 1))
aggregate(draw ~ term, data = posteriordraws(mfx), FUN = mean)

## Link scale -- Not sure

bm <- brmsmargins(
  ls.fe,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  CI = 0.95, CIType = "ETI", dpar = "sigma",
  effects = "fixedonly")
data.frame(bm$ContrastSummary)

mfx <- marginaleffects(ls.fe, type = "link", re_formula = NA)
aggregate(draw ~ term, data = posteriordraws(mfx), FUN = mean)

mfx <- marginaleffects(ls.fe, response = "link", re_formula = NA)
