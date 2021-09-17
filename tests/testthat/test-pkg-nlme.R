skip_if_not_installed("nlme")
requiet("nlme")

test_that("nlme::gls: no validity", {
    model <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
                 correlation = corAR1(form = ~ 1 | Mare))
    mfx <- marginaleffects(model)
    expect_s3_class(mfx, "data.frame")
    expect_false(any(mfx$dydx == 0 |  is.na(mfx$dydx)))
    expect_false(any(mfx$std.error == 0 |  is.na(mfx$std.error)))
})
