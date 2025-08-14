test_that("marginaleffects works with logical variables", {
    dat <- mtcars
    dat$am <- as.logical(dat$am)
    mod <- glm(vs ~ am + mpg, data = dat, family = binomial)
    mfx <- slopes(mod)
    expect_s3_class(mfx, "marginaleffects")
    expect_equal(nrow(mfx), nrow(dat) * 2)
})
