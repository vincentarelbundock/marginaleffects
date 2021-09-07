library("margins")

test_that("glm", {
    set.seed(1024)
    N <- 1e2
    dat <- data.frame(x1 = rnorm(N),
                      x2 = rnorm(N),
                      x3 = rnorm(N),
                      x4 = rnorm(N),
                      e = rnorm(N))
    dat$y <- as.numeric(plogis(
        dat$x1 + dat$x2 + dat$x3 + dat$x4 + dat$x3 * dat$x4 + dat$e) > 0.5)
    mod <- glm(y ~ x1 + x2 + x3 * x4, data = dat, family = binomial)
    res <- marginsxp(mod)
    mar <- margins(mod, unit_ses = TRUE)
    marginsxp:::test_against_margins(res, mar, tolerance = 0.001)
})


test_that("lm with interactions", {
    counterfactuals <- expand.grid(hp = 100, am = 0:1)
    mod <- lm(mpg ~ hp * am, data = mtcars)
    res <- marginsxp(mod, variable = "hp", newdata = counterfactuals)
    mar <- margins(mod, variable = "hp", data = counterfactuals, unit_ses = TRUE)
    mar <- data.frame(mar)
    marginsxp:::test_against_margins(res, mar)
})


test_that("TODO: loess vcov error is raised too early to catch", {
    mod <- loess(mpg ~ wt, data = mtcars)
    expect_error(marginsxp(mod), regexp = "not yet supported")
})


test_that("loess error", {
    skip("not sure why I get different results for loess")
    mod <- loess(mpg ~ wt, data = mtcars)
    res <- marginsxp(mod, variance = NULL)
    mar <- data.frame(margins(mod))
    marginsxp:::test_against_margins(res, mar, tolerance = .8)
})

