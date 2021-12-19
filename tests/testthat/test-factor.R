
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

test_that("factor on LHS and RHS at the same time.", {
    data(housing, package = "MASS")
    mod <- MASS::polr(Infl ~ Sat + Freq, data = housing)
    mfx <- marginaleffects(mod, type = "probs")
    expect_s3_class(mfx, "marginaleffects")
    expect_true(all(c("Low", "Medium", "High") %in% mfx$group))
})
