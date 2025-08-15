test_that("glmx package works", {
    skip_if_not_installed("glmx")
    skip_if_not_installed("MASS")
    skip_if_not_installed("margins")

    withr_library("glmx")
    withr_library("MASS")
    withr_library("margins")

    # glmx: marginaleffects vs. margins
    tmp <- data.frame(x = runif(200, -1, 1))
    tmp$y <- rnbinom(200, mu = exp(0 + 3 * tmp$x), size = 1)
    d <- tmp
    model <- glmx(y ~ x, data = d, family = negative.binomial, xlink = "log", xstart = 0)
    slo <- slopes(model)
    expect_s3_class(slo, "slopes")

    # margins produces all zeros for se
    mar <- margins(model, unit_ses = TRUE)
    mfx <- slopes(model)
    # Note: expect_margins is a custom function, using basic comparison
    expect_true(all(abs(mfx$estimate - mar$dydx) < 0.001))

    # predictions: glmx: no validity check
    # skip_if_not_installed("insight", minimum_version = "0.17.1")
    tmp <- data.frame(x = runif(200, -1, 1))
    tmp$y <- rnbinom(200, mu = exp(0 + 3 * tmp$x), size = 1)
    d <- tmp
    dhead <- head(d)
    model <- glmx(y ~ x, data = d, family = negative.binomial, xlink = "log", xstart = 0)
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = dhead)
    expect_s3_class(pred1, "predictions")
    expect_equal(nrow(pred1), nrow(d), ignore_attr = TRUE)
    expect_s3_class(pred2, "predictions")
    expect_equal(nrow(pred2), 6, ignore_attr = TRUE)
})
