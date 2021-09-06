skip_if_not_installed("lme4")

test_that("lme4: fastmargins vs. margins", {
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

    # TODO: not sure why I get different results
    N <- 1000
    tmp <- data.frame(x1 = rnorm(N),
                      x2 = rnorm(N),
                      group = sample(letters[1:10], N, replace = TRUE))
    tmp$y <- tmp$x1 + tmp$x2 + tmp$x1 * tmp$x2 + as.numeric(as.factor(tmp$group)) + rnorm(N)
    mod <- lme4::lmer(y ~ x1 + x2 + (1 | group), data = tmp)
    res <- mfx(mod, variance = NULL)
    mar <- data.frame(margins(mod))
    expect_true(cor(mar$dydx_x1, res$dydx_x1) > .999)
    expect_true(cor(mar$dydx_x2, res$dydx_x2) > .999)
    expect_error(mfx(mod), regexp = "variance.*not supported")
})
