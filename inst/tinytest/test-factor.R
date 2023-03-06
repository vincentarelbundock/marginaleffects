source("helpers.R")
using("marginaleffects")

#
#
# factor before fitting or in formula is the same
tmp <- mtcars
tmp$cyl <- factor(tmp$cyl)
mod1 <- lm(mpg ~ hp + factor(cyl), mtcars)
mod2 <- lm(mpg ~ hp + cyl, tmp)
mfx1 <- suppressWarnings(slopes(mod1))
mfx2 <- slopes(mod2)
expect_equivalent(mfx1$estimate, mfx2$estimate)
expect_equivalent(mfx1$std.error, mfx2$std.error)


# factor on LHS and RHS at the same time.
data(housing, package = "MASS")
mod <- MASS::polr(Infl ~ Sat + Freq, data = housing, Hess = TRUE)
mfx <- suppressMessages(slopes(mod, type = "probs"))
expect_inherits(mfx, "marginaleffects")
expect_true(all(c("Low", "Medium", "High") %in% mfx$group))


# smart detect factor() in formula
requiet("estimatr")
model <- lm_robust(carb ~ wt + factor(cyl), se_type = "stata", data = mtcars)
k <- slopes(model)
expect_true(all(c("dY/dX", "8 - 4") %in% k$contrast))


# factor in formula with incomplete newdata
mod <- lm(mpg ~ factor(cyl), data = mtcars)
mfx1 <- slopes(mod, newdata = data.frame(cyl = 4))
mfx2 <- slopes(mod, newdata = datagrid(cyl = 4))
expect_equivalent(mfx1[, 1:5], mfx2[, 1:5])


# bugs stay dead: get_data.coxph() with strata()
# skip_if_not_installed("insight", minimum_version = "0.17.0")
requiet("survival")
test1 <- data.frame(
    time = c(4, 3, 1, 1, 2, 2, 3),
    status = c(1, 1, 1, 0, 1, 1, 0),
    x = c(0, 2, 1, 1, 1, 0, 0),
    sex = c(0, 0, 0, 0, 1, 1, 1))
mod <- coxph(Surv(time, status) ~ x + strata(sex),
    data = test1,
    ties = "breslow")

nd <- datagrid(sex = 0, newdata = test1)
mfx <- slopes(mod,
    variables = "x",
    newdata = nd,
    type = "lp")
expect_inherits(mfx, "marginaleffects")


# Issue #497
dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat$cyl <- as.factor(dat$cyl)
mod <- lm(mpg ~ cyl, dat)
cmp1 <- comparisons(mod, variables = list(cyl = c(6, 4)))
cmp2 <- comparisons(mod, variables = list(cyl = c("4", "6")))
cmp3 <- comparisons(mod, variables = list(cyl = dat$cyl[2:3]))
expect_inherits(cmp1, "comparisons")
expect_inherits(cmp2, "comparisons")
expect_inherits(cmp3, "comparisons")


# Issue #658
dat <- transform(mtcars, cyl = factor(cyl))
mod <- lm(mpg ~ cyl, mtcars)
cmp <- comparisons(
    mod,
    variables = list(cyl = "minmax"),
    transform = function(x) x / 3)
expect_inherits(cmp, "comparisons")



rm(list = ls())