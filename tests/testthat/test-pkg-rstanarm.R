test_that("rstanarm package works", {
    skip_if_not_installed("rstanarm")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("margins")
    skip_if_not_installed("broom")

    withr_library("rstanarm")
    withr_library("emmeans")
    withr_library("margins")
    withr_library("broom")

    # HPD tests against emmeans, which uses HDI, but our default is ETI
    # HDI is implemented specifically for these tests
    # https://github.com/vincentarelbundock/marginaleffects/issues/240
    op <- getOption("marginaleffects_posterior_interval", default = "eti")
    on.exit(options(marginaleffects_posterior_interval = op))
    options("marginaleffects_posterior_interval" = "hdi")

    # interactions
    void <- capture.output(
        mod <- stan_glm(am ~ hp + mpg * vs, data = mtcars, family = binomial(link = "logit"))
    )
    slo <- slopes(mod)
    expect_s3_class(slo, "slopes")
    pre <- predictions(mod)
    expect_s3_class(pre, "predictions")

    # no interactions
    void <- capture.output(
        mod <- stan_glm(am ~ hp + mpg + vs, data = mtcars, family = binomial(link = "logit"))
    )

    # emtrends
    mfx <- slopes(mod, newdata = datagrid(hp = 110, mpg = 20, vs = 0), variables = "hp", type = "link")
    em <- emtrends(mod, ~hp, "hp", at = list(hp = 110, mpg = 20, vs = 0))
    em <- tidy(em)
    expect_equal(mfx$estimate, em$hp.trend, ignore_attr = TRUE)
    expect_equal(mfx$conf.low, em$lower.HPD, tolerance = 1e-5, ignore_attr = TRUE)
    expect_equal(mfx$conf.high, em$upper.HPD, ignore_attr = TRUE)

    # # margins: var is all zeroes and dydx don't match precisely
    # mar <- margins(mod, unit_ses = TRUE, at = list(hp = 110, mpg = 20, vs = 0))
    # mfx <- slopes(mod, variables = "hp", at = list(hp = 110, mpg = 20, vs = 0))
    # expect_equal(mfx$estimate, mar$dydx_hp, ignore_attr = TRUE)
    # expect_equal(mfx$std.error, mar$dydx_hp, ignore_attr = TRUE)
})
