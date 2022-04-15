requiet("sandwich")

test_that("working but no validity check", {
    mod <- lm(mpg ~ hp + drat, mtcars)
    a <- tidy(marginaleffects(mod))
    b <- tidy(marginaleffects(mod, vcov = sandwich::vcovHC(mod)))
    expect_true(all(a$estimate == b$estimate))
    expect_true(all(a$std.error != b$std.error))
})


test_that("matrix produces different results (no validity)", {
    mod <- lm(mpg ~ hp * wt, data = mtcars)
    V <- vcovHC(mod)
    mfx1 <- marginaleffects(mod)
    mfx2 <- marginaleffects(mod, vcov = V)
    expect_true(all(mfx1$std.error != mfx2$std.error))
    pre1 <- predictions(mod)
    pre2 <- predictions(mod, vcov = V)
    expect_true(all(pre1$std.error != pre2$std.error))
    cmp1 <- comparisons(mod)
    cmp2 <- comparisons(mod, vcov = V)
    expect_true(all(cmp1$std.error != cmp2$std.error))
})
