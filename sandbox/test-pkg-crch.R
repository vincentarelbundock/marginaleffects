skip_if_not_installed("crch")
library("crch")

test_that("crch: no validity", {
    data("RainIbk", package = "crch")
    ## mean and standard deviation of square root transformed ensemble forecasts
    RainIbk$sqrtensmean <- apply(sqrt(RainIbk[,grep('^rainfc',names(RainIbk))]), 1, mean)
    RainIbk$sqrtenssd <- apply(sqrt(RainIbk[,grep('^rainfc',names(RainIbk))]), 1, sd)
    ## fit linear regression model with Gaussian distribution 
    model <- crch(sqrt(rain) ~ sqrtensmean + sqrtenssd, data = RainIbk, dist = "gaussian")
    mfx <- marginaleffects(model)
    tid <- tidy(mfx)
    expect_false(length(unique(mfx$dydx)) == 1)
    expect_false(anyNA(mfx$dydx))
    expect_false(any(mfx$dydx == 0))
    expect_false(any(mfx$std.error == 0))
    expect_false(anyNA(mfx$std.error))
    expect_s3_class(mfx, "data.frame")
    expect_s3_class(tid, "data.frame")
})

test_that("trch: no validity", {
})
