source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("rms")
requiet("emmeans")
requiet("broom")

# lmr: marginaleffects vs emtrends
model <- rms::lrm(am ~ mpg, mtcars)
void <- capture.output({
    expect_marginaleffects(model, type = "lp", n_unique = 1)
})
mfx <- marginaleffects(model, newdata = data.frame(mpg = 30), type = "lp")
em <- emtrends(model, ~mpg, "mpg", at = list(mpg = 30))
em <- tidy(em)
expect_equivalent(mfx$dydx, em$mpg.trend)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .0001)


# predictions: rms: no validity
model <- rms::lrm(am ~ mpg, mtcars)
pred1 <- predictions(model, type = "lp")
pred2 <- predictions(model, type = "lp", newdata = head(mtcars))
expect_predictions(pred1, n_row = 32)
expect_predictions(pred2, n_row = 6)
