library(margins)
library(fastmargins)
library(ggeffects)

test_that("glm", {
    N <- 1e2
    dat <- data.frame(x1 = rnorm(N),
                      x2 = rnorm(N),
                      x3 = rnorm(N),
                      x4 = rnorm(N),
                      e = rnorm(N))
    dat$y <- as.numeric(plogis(
        dat$x1 + dat$x2 + dat$x3 + dat$x4 + dat$x3 * dat$x4 + dat$e) > 0.5)
    mod <- glm(y ~ x1 + x2 + x3 * x4, data = dat, family = binomial)
    unknown <- mfx(mod, variance = vcov(mod))
    known <- data.frame(margins(mod, unit_ses = TRUE))
    expect_true(cor(unknown$se_dydx_x1, known$SE_dydx_x1) > 0.999)
    expect_true(cor(unknown$se_dydx_x2, known$SE_dydx_x2) > 0.999)
    expect_true(cor(unknown$se_dydx_x3, known$SE_dydx_x3) > 0.999)
    expect_true(cor(unknown$se_dydx_x4, known$SE_dydx_x4) > 0.999)
})

test_that("lm with interactions", {
    counterfactuals <- expand.grid(hp = 100, am = 0:1)
    mod <- lm(mpg ~ hp * am, data = mtcars)
    res <- mfx(mod, variable = "hp", fitfram = counterfactuals)
    mar <- margins(mod, variable = "hp", data = counterfactuals, unit_ses = TRUE)
    mar <- data.frame(mar)
    expect_equal(res$dydx_hp, as.numeric(mar$dydx_hp))
    expect_equal(res$se_dydx_hp, as.numeric(mar$SE_dydx_hp), tolerance = 1e-4)
})

test_that("fixest", {
    skip_if_not_installed("fixest")
    library("fixest")

    # logit is identical
    counterfactuals <- data.frame(hp = 110, wt = c(min(mtcars$wt), max(mtcars$wt)), cyl = 4)
    mod1 = feglm(am ~ hp * wt, data = mtcars, family = "binomial")
    mod2 = glm(am ~ hp * wt, data = mtcars, family = "binomial")
    expect_equal(mfx(mod1, fitfram = counterfactuals),
                 mfx(mod2, fitfram = counterfactuals),
                 tolerance = 1e-3)

    # TODO: this only checks if it outputs a data.frame, not if the results are correct
    mod = feglm(am ~ hp * wt | cyl, data = mtcars, family = "binomial")
    res = mfx(mod, fitfram = counterfactuals)
    expect_s3_class(res, "data.frame")
})

test_that("loess", {
    mod <- loess(mpg ~ wt, data = mtcars)
    expect_error(mfx(mod), regexp = "not supported")
    res <- mfx(mod, variance = NULL)
    mar <- data.frame(margins(mod))
    expect_true(cor(as.numeric(mar$dydx_wt), res$dydx_wt, use = "complete.obs") > .99999)
})

test_that("betareg", {
    skip_if_not_installed("betareg")
    library(betareg)
    data("GasolineYield", package = "betareg")
    mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    suppressWarnings({
        res <- mfx(mod, variables = "temp", variance = NULL)
        mar <- data.frame(margins(mod, unit_ses = FALSE))
    })
    expect_true(cor(as.numeric(mar$temp), res$temp, use = "complete.obs") > .99999)
    # TODO: variance does not work for betareg objects
})

test_that("multinom", {
    skip_if_not_installed("nnet")
    library(nnet)
    tmp <- mtcars
    tmp$cyl <- as.factor(tmp$cyl)
    void <- capture.output( mod <- 
        nnet::multinom(cyl ~ hp + am + mpg, data = tmp, quiet = true))
    res <- mfx(mod, group_names = c("4", "6"), variance = NULL)
    expect_s3_class(res, "data.frame")

    # TODO: `margins` appears to break with numeric regressors but not factors
    # here it's the opposite: we don't support factors.
    expect_error(margins(mod))
})

test_that("polr", {
    skip_if_not_installed("MASS")
    library(MASS)
    tmp <- mtcars
    tmp$carb <- as.factor(tmp$carb)
    mod <- polr(carb ~ hp + am + mpg, data = tmp) 
    res <- mfx(mod, group_names = c("1", "2", "3", "4", "6"), variance = NULL)
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(32, 19))
    # TODO: not supported yet
    expect_error(mfx(mod, variance = NULL), regexp = "must specify")
    expect_error(mfx(mod, group_names = "1"), regexp = "not supported")
})

test_that("merMod", {
    skip_if_not_installed("lme4")
    N <- 1000
    tmp <- data.frame(x1 = rnorm(N),
                      x2 = rnorm(N),
                      y = sample(0:1, N, replace = TRUE),
                      group = sample(letters[1:10], N, replace = TRUE))
    mod <- lme4::glmer(y ~ x1 + x2 + (1 | group), data = tmp, family = binomial)
    res <- mfx(mod, variance = NULL)
    mar <- data.frame(margins(mod))
    expect_true(cor(mar$dydx_x1, res$dydx_x1) > .999)
    expect_true(cor(mar$dydx_x2, res$dydx_x2) > .999)
    expect_error(mfx(mod), regexp = "variance.*not supported")
})
