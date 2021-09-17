skip_if(getRversion() < 4.0)

test_that("simple summary output", {
    mod <- lm(mpg ~ hp + factor(cyl), mtcars)
    mfx <- marginaleffects(mod)
    s <- summary(mfx)
    expect_snapshot(print(summary(mfx), digits = 3))
})
