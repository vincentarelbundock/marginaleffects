requiet("quantreg")
requiet("emmeans")
requiet("broom")

test_that("marginaleffects: rq: Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$quantreg_rq_01
    model <- quantreg::rq(mpg ~ hp * wt + factor(cyl), data = mtcars)
    expect_marginaleffects(model)
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)
})

test_that("marginaleffects vs. emtrends", {
    stata <- readRDS(test_path("stata/stata.rds"))$quantreg_rq_01
    model <- quantreg::rq(mpg ~ hp * wt + factor(cyl), data = mtcars)
    mfx <- marginaleffects(model, variables = "hp", newdata = datagrid(hp = 110, wt = 2, cyl = 4))
    em <- emtrends(model, ~hp, "hp", at = list(hp = 110, wt = 2, cyl = 4))
    em <- tidy(em)
    expect_equal(mfx$dydx, em$hp.trend, tolerance = .00001)
    expect_equal(mfx$std.error, em$std.error, tolerance = .00001)
})

test_that("predictions: rq: no validity", {
    model <- quantreg::rq(mpg ~ hp * wt + factor(cyl), data = mtcars)
    pred1 <- predictions(model)
    pred2 <- suppressWarnings(predictions(model, newdata = head(mtcars)))
    expect_predictions(pred1, n_row = nrow(mtcars))
    expect_predictions(pred2, n_row = 6)
})

test_that("marginalmeans: rq: no validity", {
    tmp <- mtcars
    tmp$cyl <- factor(tmp$cyl)
    model <- quantreg::rq(mpg ~ hp + wt + cyl, data = tmp)
    mm <- marginalmeans(model)
    expect_marginalmeans(mm)
})
