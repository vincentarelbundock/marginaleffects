source("helpers.R")
using("marginaleffects")

# examples from the main documentation
mod <- lm(mpg ~ hp, data = mtcars)
cmp <- comparisons(mod, variables = list(hp = c(90, 110)))
expect_inherits(cmp, "comparisons")


# Issue #527
dat <- mtcars
dat$new_hp <- 49 * (dat$hp - min(dat$hp)) / (max(dat$hp) - min(dat$hp)) + 1
dat <- dat
mod <- lm(mpg ~ log(new_hp) + factor(cyl), data = dat)
fdiff <- function(x) data.frame(x, x + 10)
cmp1 <- comparisons(mod, variables = list(new_hp = fdiff))
cmp2 <- comparisons(mod, variables = list(new_hp = 10))
expect_equivalent(nrow(cmp1), 32)
expect_equivalent(nrow(cmp2), 32)


# Issue #720
mod <- lm(mpg ~ hp * qsec, dat = mtcars)
cmp <- avg_comparisons(mod, variables = list(hp = "2sd"))
expect_equivalent(cmp$contrast, "mean(x + sd) - mean(x - sd)")



# Issue #622 cross-contrasts
mod <- lm(mpg ~ am * factor(cyl), data = mtcars)
cmp <- comparisons(mod, variables = c("cyl", "am"), cross = TRUE)
expect_equivalent(nrow(cmp), 64)
cmp <- avg_comparisons(mod, variables = c("cyl", "am"), cross = TRUE)
expect_equivalent(nrow(cmp), 2)



# Issue #794
mod <- glm(am ~ hp, data = mtcars, family = binomial())
cmp1 <- comparisons(mod, comparison = "lift")
cmp2 <- comparisons(mod, comparison = "liftavg")
expect_equal(nrow(cmp1), 32)
expect_equal(nrow(cmp2), 1)
expect_error(comparisons(mod, comparison = "liftr"))


# Issue #1120: avg comparison by default with avg_
mod <- glm(vs ~ am + wt, data = mtcars, family = binomial)
d0 <- transform(mtcars, am = 0)
d1 <- transform(mtcars, am = 1)
p0 <- predict(mod, newdata = d0, type = "response")
p1 <- predict(mod, newdata = d1, type = "response")
c1 <- mean(p1 / p0) # marginaleffects 0.20.0
c2 <- mean(p1) / mean(p0) # after bug fix
cmp <- avg_comparisons(mod, variables = "am", comparison = "ratio")
expect_equivalent(cmp$estimate, c2)



# Issue #1137
fit <- lm(mpg ~ vs + am + vs:am, data=mtcars) # equivalent to ~ vs*am
cmp <- avg_comparisons(fit, variables = list(am = c(0, 1), vs = c(1, 0)), cross = TRUE)
expect_equivalent(cmp$estimate, -0.992857142857154)
expect_error(avg_comparisons(fit, variables = list(am = 0, vs = 1:0), cross = TRUE), "length 2")
expect_error(avg_comparisons(fit, variables = list(am = 1:3, vs = 1:0), cross = TRUE), "0 or 1")



rm(list = ls())