skip_if_not_installed("aod")
withr_library("aod")

test_that("aod package betabin works with factor variables", {
    dat <- get_dataset("orob2", "aod")

    # character variables should be padded, but I am lazy
    mod <- betabin(cbind(y, n - y) ~ seed, ~1, data = dat)
    expect_error(slopes(mod), regexp = "factors")

    # factor variables work
    dat$seed <- factor(dat$seed)
    mod <- betabin(cbind(y, n - y) ~ seed, ~1, data = dat)
    expect_s3_class(slopes(mod), "slopes")

    pre <- predictions(mod)
    expect_s3_class(pre, "predictions")
})
