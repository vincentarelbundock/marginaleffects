test_that("crch package works", {
    skip_if_not_installed("crch")
    skip_if_not_installed("ordinal")

    withr_library("crch")
    withr_library("ordinal")

    dat <- get_dataset("RainIbk", "crch")
    q <- unique(stats::quantile(dat$rain, seq(0.1, 0.9, 0.1)))
    dat$rain_sqrt <- sqrt(dat$rain)
    dat$sqrtensmean <- apply(sqrt(dat[, grep('^rainfc', names(dat))]), 1, mean)
    dat$sqrtenssd <- apply(sqrt(dat[, grep('^rainfc', names(dat))]), 1, sd)
    dat$enssd <- apply(dat[, grep('^rainfc', names(dat))], 1, sd)
    dat$ensmean <- apply(dat[, grep('^rainfc', names(dat))], 1, mean)
    dat <<- subset(dat, enssd > 0)

    # marginaleffects: crch gaussian: no validity
    model <- crch(sqrt(rain) ~ sqrtensmean + sqrtenssd, data = dat, dist = "gaussian")
    slo <- slopes(model, type = "location")
    expect_s3_class(slo, "slopes")

    # logistic: no validity
    model <- crch(sqrt(rain) ~ sqrtensmean | sqrtenssd, data = dat, dist = "logistic", left = 0)
    slo <- slopes(model, type = "location")
    expect_s3_class(slo, "slopes")

    mfx <- slopes(model, type = "location", variables = "sqrtensmean")
    expect_true(!any(mfx$estimate == 0))

    mfx <- slopes(model, type = "location", variables = "sqrtenssd")
    expect_true(all(mfx$estimate == 0))

    mfx <- slopes(model, type = "scale", variables = "sqrtensmean")
    expect_true(all(mfx$estimate == 0))

    mfx <- slopes(model, type = "scale", variables = "sqrtenssd")
    expect_true(!any(mfx$estimate == 0))

    # hlxr: no validity
    mod <- hxlr(rain_sqrt ~ sqrtensmean, data = dat, thresholds = sqrt(q))
    slo <- slopes(mod, type = "location")
    expect_s3_class(slo, "slopes")

    # predictions: crch gaussian: no validity
    model <- crch(sqrt(rain) ~ sqrtensmean + sqrtenssd, data = dat, dist = "gaussian")
    pred1 <- predictions(model, newdata = dat)
    pred2 <- predictions(model, newdata = head(dat))
    expect_s3_class(pred1, "predictions")
    expect_equal(nrow(pred1), nrow(dat), ignore_attr = TRUE)
    expect_s3_class(pred2, "predictions")
    expect_equal(nrow(pred2), 6, ignore_attr = TRUE)
})
