test_that("rms package works", {
    skip_if_not_installed("polspline")
    skip_if_not_installed("rms")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("polspline")
    withr_library("rms")
    withr_library("emmeans")
    withr_library("broom")

    # lmr: marginaleffects vs emtrends
    model <- rms::lrm(am ~ mpg, mtcars)
    void <- capture.output({
        slo <- slopes(model, type = "lp")
        expect_s3_class(slo, "slopes")
    })
    mfx <- slopes(model, newdata = data.frame(mpg = 30), type = "lp", eps = 1 / 1000 * diff(range(mtcars$mpg)))
    em <- emtrends(model, ~mpg, "mpg", at = list(mpg = 30))
    em <- tidy(em)
    expect_equal(mfx$estimate, em$mpg.trend, ignore_attr = TRUE)
    expect_equal(mfx$std.error, em$std.error, tolerance = .0001, ignore_attr = TRUE)

    # predictions: rms: no validity
    model <- rms::lrm(am ~ mpg, mtcars)
    pred1 <- predictions(model, type = "lp")
    pred2 <- predictions(model, type = "lp", newdata = head(mtcars))
    expect_s3_class(pred1, "predictions")
    expect_equal(nrow(pred1), 32, ignore_attr = TRUE)
    expect_s3_class(pred2, "predictions")
    expect_equal(nrow(pred2), 6, ignore_attr = TRUE)

    # comparisons
    mod <- ols(mpg ~ hp, mtcars)
    c1 <- comparisons(mod, type = "lp")
    expect_s3_class(c1, "comparisons")

    mod <- lrm(am ~ hp, mtcars)
    c1 <- comparisons(mod, type = "fitted")
    c2 <- comparisons(mod, type = "lp")
    expect_s3_class(c1, "comparisons")
    expect_s3_class(c2, "comparisons")

    mod <- lrm(cyl ~ hp, mtcars)
    c1 <- comparisons(mod, type = "fitted")
    c2 <- comparisons(mod, type = "lp")
    expect_s3_class(c1, "comparisons")
    expect_s3_class(c2, "comparisons")

    mod <- orm(cyl ~ hp, mtcars)
    c1 <- comparisons(mod, type = "fitted")
    c2 <- comparisons(mod, type = "lp")
    c3 <- comparisons(mod, type = "mean")
    expect_s3_class(c1, "comparisons")
    expect_s3_class(c2, "comparisons")

    expect_error(comparisons(mod, vcov = "HC3"), pattern = "supported")

    # Issue #1428
    skip_if_not_installed("tibble")
    withr_library("tibble")
    data <- tibble::tibble(
        y = rbinom(100, 1, .4),
        x1 = rnorm(100),
        x2 = rnorm(100),
        x3 = rep(c("A", "B"), 50)
    )
    f <- lrm(y ~ ., data = data)
    p <- suppressWarnings(get_predict(f))
    expect_s3_class(p, "data.frame")
    expect_equal(dim(p), c(100, 2), ignore_attr = TRUE)
    expect_warning(get_predict(f), pattern = "Converting.*tibble")
})
