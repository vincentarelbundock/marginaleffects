skip_if_not_installed("crch")
requiet("crch")


data("RainIbk", package = "crch")
dat <- RainIbk
dat$sqrtensmean <- apply(sqrt(dat[,grep('^rainfc',names(dat))]), 1, mean)
dat$sqrtenssd <- apply(sqrt(dat[,grep('^rainfc',names(dat))]), 1, sd)
dat$enssd <- apply(dat[,grep('^rainfc',names(dat))], 1, sd)
dat$ensmean <- apply(dat[,grep('^rainfc',names(dat))], 1, mean)
dat <- subset(dat, enssd > 0)


test_that("crch gaussian: no validity", {
    model <- crch(sqrt(rain) ~ sqrtensmean + sqrtenssd, data = dat, dist = "gaussian")
    expect_mfx(model, n_unique = 1, type = "location")
})


test_that("logistic: no validity", {
    model <- crch(sqrt(rain) ~ sqrtensmean | sqrtenssd, data = dat, dist = "logistic", left = 0)
    expect_mfx(model, type = "location", n_unique = 1)

    mfx <- marginaleffects(model, type = "location", variables = "sqrtensmean")
    expect_true(!any(mfx$dydx == 0))

    mfx <- marginaleffects(model, type = "location", variables = "sqrtenssd")
    expect_true(all(mfx$dydx == 0))

    mfx <- marginaleffects(model, type = "scale", variables = "sqrtensmean")
    expect_true(all(mfx$dydx == 0))

    mfx <- marginaleffects(model, type = "scale", variables = "sqrtenssd")
    expect_true(!any(mfx$dydx == 0))
})


test_that("hlxr: no validity", {
    skip("works in interactive session")
    q <- unique(quantile(dat$rain, seq(0.1, 0.9, 0.1)))
    mod <- hxlr(sqrt(rain) ~ sqrtensmean, data = dat, thresholds = sqrt(q))
    expect_mfx(mod, type = "location", n_unique = 1)
})
