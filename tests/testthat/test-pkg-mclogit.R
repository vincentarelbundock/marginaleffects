skip_if_not_installed("mclogit")
requiet("mclogit")
requiet("MASS")
requiet("emmeans")
requiet("broom")


test_that("mclogit: no validity", {
    data(Transport, package = "mclogit")
    void <- capture.output(
        model <- mclogit(cbind(resp, suburb) ~ distance + cost, data = Transport)
    )

    # type = "response" produces 0 dydx and standard error. Not sure why
    # because `get_predict(newdata)` seems to work
    expect_error(marginaleffects(model, type = "response"), regexp = "type. argument")

    expect_marginaleffects(model, type = "link", n_unique = 1)
    pred <- predictions(model, type = "link")
    expect_predictions(pred)
})


test_that("mblogit: no validity", {
    data("housing", package = "MASS")
    dat <- housing
    dat$x <- rnorm(nrow(dat))
    void <- capture.output(
        mod <- mblogit(Sat ~ Infl + Type + Cont + x, weights = Freq, data = dat)
    )
    expect_predictions(predictions(mod))
    expect_marginaleffects(mod)
})


test_that("mblogit: marginaleffects vs. emmeans", {
    skip("emmeans does not support")
    mm <- marginalmeans(mod, variables = c("Infl"))
    em <- emmeans(mod, ~Infl)
    em <- tidy(emmeans::emtrends(mod, ~x, "x"))
    mfx <- marginaleffects(mod)
})
