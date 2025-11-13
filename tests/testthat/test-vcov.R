testthat::skip_if_not_installed("sandwich")
requiet("sandwich")


dat_vcov <- get_dataset("mtcars", "datasets")

# working but no validity check
mod <- lm(mpg ~ hp + drat, data = dat_vcov)
a <- tidy(slopes(mod))

assign("tmp", vcovHC(mod), envir = .GlobalEnv)

mfx <- slopes(mod, vcov = tmp)
b <- tidy(mfx)
expect_true(all(a$estimate == b$estimate))
expect_true(all(a$std.error != b$std.error))

rm("tmp", envir = .GlobalEnv)


# matrix produces different results (no validity)
mod <- lm(mpg ~ hp * wt, data = dat_vcov)
V <- vcovHC(mod)
mfx1 <- slopes(mod)
mfx2 <- slopes(mod, vcov = V)
expect_true(all(mfx1$std.error != mfx2$std.error))
pre1 <- predictions(mod)
pre2 <- predictions(mod, vcov = V)
expect_true(all(pre1$std.error != pre2$std.error))
cmp1 <- comparisons(mod)
cmp2 <- comparisons(mod, vcov = V)
expect_true(all(cmp1$std.error != cmp2$std.error))


# marginaleffects strings (no validity)
mod <- lm(mpg ~ hp * wt, data = dat_vcov)

# aliases
mfx1 <- slopes(mod, vcov = "HC2")
mfx2 <- slopes(mod, vcov = "stata")
mfx3 <- slopes(mod, vcov = "HC3")
mfx4 <- slopes(mod, vcov = "robust")
expect_equal(mfx1$std.error, mfx2$std.error, ignore_attr = TRUE)
expect_equal(mfx3$std.error, mfx4$std.error, ignore_attr = TRUE)

# different (no validity)
mfx5 <- slopes(mod, vcov = ~cyl)
mfx6 <- slopes(mod, vcov = "HAC")
expect_true(all(mfx1$std.error != mfx3$std.error))
expect_true(all(mfx1$std.error != mfx4$std.error))
expect_true(all(mfx1$std.error != mfx5$std.error))
expect_true(all(mfx1$std.error != mfx6$std.error))
expect_true(all(mfx3$std.error != mfx5$std.error))
expect_true(all(mfx3$std.error != mfx6$std.error))
expect_true(all(mfx5$std.error != mfx6$std.error))


# predictions strings (no validity)
mod <- lm(mpg ~ hp * wt, data = dat_vcov)

# aliases
pre1 <- predictions(mod, vcov = "HC2")
pre2 <- predictions(mod, vcov = "stata")
pre3 <- predictions(mod, vcov = "HC3")
pre4 <- predictions(mod, vcov = "robust")
expect_equal(pre1$std.error, pre2$std.error, ignore_attr = TRUE)
expect_equal(pre3$std.error, pre4$std.error, ignore_attr = TRUE)

# different (no validity)
pre5 <- predictions(mod, vcov = ~cyl)
pre6 <- predictions(mod, vcov = "HAC")
expect_true(all(pre1$std.error != pre3$std.error))
expect_true(all(pre1$std.error != pre4$std.error))
expect_true(all(pre1$std.error != pre5$std.error))
expect_true(all(pre1$std.error != pre6$std.error))
expect_true(all(pre3$std.error != pre5$std.error))
expect_true(all(pre3$std.error != pre6$std.error))
expect_true(all(pre5$std.error != pre6$std.error))


# Issue #554
mod <- lm(mpg ~ cyl, data = dat_vcov)

x <- get_vcov(mod, vcov = sandwich::vcovHC)
y <- get_vcov(mod, vcov = "HC3")
expect_equal(x, y, ignore_attr = TRUE)

x <- slopes(mod, vcov = sandwich::vcovHC)
y <- slopes(mod, vcov = "HC3")
expect_equal(x, y, ignore_attr = TRUE)
