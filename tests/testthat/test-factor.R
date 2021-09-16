test_that("factor before fitting or in formula is the same", {
    tmp <- mtcars
    tmp$cyl <- factor(tmp$cyl)
    mod1 <- lm(mpg ~ hp + factor(cyl), mtcars)
    mod2 <- lm(mpg ~ hp + cyl, tmp)
    mfx1 <- marginaleffects(mod1)
    mfx2 <- marginaleffects(mod2)
    expect_equal(mfx1$estimate, mfx2$estimate)
    expect_equal(mfx1$std.error, mfx2$std.error)
})
