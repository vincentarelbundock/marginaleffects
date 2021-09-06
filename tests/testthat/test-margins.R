library(margins)
library(ggeffects)

test_that("logit model gives same results in `fastmfx` and `margins`", {
    N <- 1e2
    dat <- data.frame(x1 = rnorm(N),
                      x2 = rnorm(N),
                      x3 = rnorm(N),
                      x4 = rnorm(N),
                      e = rnorm(N))
    dat$y <- as.numeric(plogis(dat$x1 + dat$x2 + dat$x3 + dat$x4 + 
                              dat$x3 * dat$x4 + dat$e) > 0.5)
    mod <- glm(y ~ x1 + x2 + x3 * x4, data = dat, family = binomial)
    unknown <- mfx(mod, variance = vcov(mod))
    known <- data.frame(margins(mod, unit_ses = TRUE))
    expect_true(cor(unknown$se_dydx_x1, known$SE_dydx_x1) > 0.999)
    expect_true(cor(unknown$se_dydx_x2, known$SE_dydx_x2) > 0.999)
    expect_true(cor(unknown$se_dydx_x3, known$SE_dydx_x3) > 0.999)
    expect_true(cor(unknown$se_dydx_x4, known$SE_dydx_x4) > 0.999)
})

test_that("linear regression with interactions", {
    counterfactuals <- expand.grid(hp = 100, am = 0:1)
    mod <- lm(mpg ~ hp * am, data = mtcars)
    res <- mfx(mod, variable = "hp", fitfram = dat)
    mar <- margins(mod, variable = "hp", data = counterfactuals, unit_ses = TRUE)
    mar <- data.frame(mar)
    expect_equal(res$dydx_hp, as.numeric(mar$dydx_hp))
    expect_equal(res$se_dydx_hp, as.numeric(mar$SE_dydx_hp), tolerance = 1e-4)
})
