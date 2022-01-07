requiet("rms")
requiet("emmeans")
requiet("broom")

test_that("lmr: marginaleffects vs emtrends", {
    model <- rms::lrm(am ~ mpg, mtcars)
    void <- capture.output({
        expect_marginaleffects(model, type = "lp", n_unique = 1)
    })
    mfx <- marginaleffects(model, newdata = data.frame(mpg = 30), type = "lp")
    em <- emtrends(model, ~mpg, "mpg", at = list(mpg = 30))
    em <- tidy(em)
    expect_equal(mfx$dydx, em$mpg.trend)
    expect_equal(mfx$std.error, em$std.error, tolerance = .0001)
})

test_that("predictions: rms: no validity", {
    skip("unsupported data argument in get_predicted.lrm")
    model <- rms::lrm(am ~ mpg, mtcars)
    pred1 <- predictions(model, type = "lp")
    pred2 <- predictions(model, type = "lp", newdata = head(mtcars))
    expect_predictions(pred1, n_row = 1)
    expect_predictions(pred2, n_row = 6)
})

