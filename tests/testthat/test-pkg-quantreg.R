test_that("quantreg package works", {
    skip_if_not_installed("quantreg")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("quantreg")
    withr_library("emmeans")
    withr_library("broom")

    # marginaleffects: rq: Stata
    stata <- readRDS(test_path("stata/stata.rds"))$quantreg_rq_01
    model <- suppressWarnings(quantreg::rq(mpg ~ hp * wt + factor(cyl), data = mtcars))
    slo <- slopes(model)
    expect_s3_class(slo, "slopes")
    mfx <- merge(avg_slopes(model), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001, ignore_attr = TRUE)

    # marginaleffects vs. emtrends
    stata <- readRDS(test_path("stata/stata.rds"))$quantreg_rq_01
    model <- quantreg::rq(mpg ~ hp * wt + factor(cyl), data = mtcars)
    mfx <- slopes(model, variables = "hp", newdata = datagrid(hp = 110, wt = 2, cyl = 4))
    em <- suppressMessages(emtrends(model, ~hp, "hp", at = list(hp = 110, wt = 2, cyl = 4)))
    em <- tidy(em)
    expect_equal(mfx$estimate, em$hp.trend, tolerance = .00001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)

    # predictions: rq: no validity
    model <- quantreg::rq(mpg ~ hp * wt + factor(cyl), data = mtcars)
    pred1 <- predictions(model)
    pred2 <- suppressWarnings(predictions(model, newdata = head(mtcars)))
    expect_s3_class(pred1, "predictions")
    expect_equal(nrow(pred1), nrow(mtcars), ignore_attr = TRUE)
    expect_s3_class(pred2, "predictions")
    expect_equal(nrow(pred2), 6, ignore_attr = TRUE)
    expect_equal(pred1$estimate, predict(model), ignore_attr = TRUE)
    expect_equal(pred2$estimate, predict(model, newdata = head(mtcars)), ignore_attr = TRUE)

    # Issue #829
    mod = rq(Sepal.Length ~ Sepal.Width * Petal.Length + Species, tau = .25, data = iris)
    cmp = comparisons(mod)
    expect_false(any(is.na(cmp$Species)))
    expect_false(any(is.na(iris$Species)))
})
