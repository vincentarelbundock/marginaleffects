skip_if_not_installed("mclogit")
requiet("mclogit")
requiet("MASS")
requiet("emmeans")
requiet("broom")


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
