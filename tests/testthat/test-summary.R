test_that("simple summary output", {
    mod <- lm(mpg ~ hp + factor(cyl), mtcars)
    mfx <- marginaleffects(mod)
    expect_snapshot(summary(mfx))
})
