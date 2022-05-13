requiet("sandwich")
skip_if_not_installed("insight", minimum_version = "0.17.1")

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


test_that("marginaleffects strings (no validity)", {
    mod <- lm(mpg ~ hp * wt, data = mtcars)

    # aliases
    mfx1 <- marginaleffects(mod, vcov = "HC2")
    mfx2 <- marginaleffects(mod, vcov = "stata")
    mfx3 <- marginaleffects(mod, vcov = "HC3")
    mfx4 <- marginaleffects(mod, vcov = "robust")
    expect_equal(mfx1$std.error, mfx2$std.error)
    expect_equal(mfx3$std.error, mfx4$std.error)

    # different (no validity)
    mfx5 <- marginaleffects(mod, vcov = ~ cyl)
    mfx6 <- marginaleffects(mod, vcov = "HAC")
    expect_true(all(mfx1$std.error != mfx3$std.error))
    expect_true(all(mfx1$std.error != mfx4$std.error))
    expect_true(all(mfx1$std.error != mfx5$std.error))
    expect_true(all(mfx1$std.error != mfx6$std.error))
    expect_true(all(mfx3$std.error != mfx5$std.error))
    expect_true(all(mfx3$std.error != mfx6$std.error))
    expect_true(all(mfx5$std.error != mfx6$std.error))
})


test_that("predictions strings (no validity)", {
    mod <- lm(mpg ~ hp * wt, data = mtcars)

    # aliases
    pre1 <- predictions(mod, vcov = "HC2")
    pre2 <- predictions(mod, vcov = "stata")
    pre3 <- predictions(mod, vcov = "HC3")
    pre4 <- predictions(mod, vcov = "robust")
    expect_equal(pre1$std.error, pre2$std.error)
    expect_equal(pre3$std.error, pre4$std.error)

    # different (no validity)
    pre5 <- predictions(mod, vcov = ~ cyl)
    pre6 <- predictions(mod, vcov = "HAC")
    expect_true(all(pre1$std.error != pre3$std.error))
    expect_true(all(pre1$std.error != pre4$std.error))
    expect_true(all(pre1$std.error != pre5$std.error))
    expect_true(all(pre1$std.error != pre6$std.error))
    expect_true(all(pre3$std.error != pre5$std.error))
    expect_true(all(pre3$std.error != pre6$std.error))
    expect_true(all(pre5$std.error != pre6$std.error))
})


test_that("marginalmeans strings (no validity)", {
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    mod <- lm(mpg ~ cyl, data = dat)

    # aliases
    mm1 <- marginalmeans(mod, vcov = "HC2")
    mm2 <- marginalmeans(mod, vcov = "stata")
    mm3 <- marginalmeans(mod, vcov = "HC3")
    mm4 <- marginalmeans(mod, vcov = "robust")
    expect_equal(mm1$std.error, mm2$std.error)
    expect_equal(mm3$std.error, mm4$std.error)

    # different (no validity)
    mm5 <- marginalmeans(mod, vcov = ~ gear)
    mm6 <- marginalmeans(mod, vcov = "HAC")
    expect_true(all(mm1$std.error != mm3$std.error))
    expect_true(all(mm1$std.error != mm4$std.error))
    expect_true(all(mm1$std.error != mm5$std.error))
    expect_true(all(mm1$std.error != mm6$std.error))
    expect_true(all(mm3$std.error != mm5$std.error))
    expect_true(all(mm3$std.error != mm6$std.error))
    expect_true(all(mm5$std.error != mm6$std.error))
})
