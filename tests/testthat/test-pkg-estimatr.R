skip_if_not_installed("estimatr")

requiet("estimatr")

test_that("iv_robust vs. stata", {
    data(Kmenta, package = "ivreg")
    model <- estimatr::iv_robust(Q ~ P + D | D + F + A, data = Kmenta)
    stata <- readRDS(test_path("stata/stata.rds"))$estimatr_iv_robust
    mfx <- tidy(marginaleffects(model))
    mfx <- merge(mfx, stata)
    expect_equal(mfx$dydx, mfx$dydxstata)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .1)
})


test_that("lm_robust vs. stata", {
    model <- estimatr::lm_robust(carb ~ wt + factor(cyl), mtcars)
    stata <- readRDS(test_path("stata/stata.rds"))$estimatr_lm_robust
    mfx <- tidy(marginaleffects(model))
    mfx <- merge(mfx, stata)
    expect_equal(mfx$dydx, mfx$dydxstata)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .1)
})
