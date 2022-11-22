source("helpers.R", local = TRUE)

# print.comparisons.summary
dat <- mtcars
dat$gear <- factor(dat$gear)
mod <- lm(mpg ~ gear, data = dat)
cmp <- comparisons(mod)

known <- " Term Contrast Effect Std. Error z value   Pr(>|z|)  2.5 % 97.5 %
1 gear    4 - 3  8.427      1.823   4.621 3.8123e-06 4.8528  12.00
2 gear    5 - 3  5.273      2.431   2.169   0.030082 0.5082  10.04

Model type:  lm
Prediction type:  response"
expect_print(summary(cmp), known)


# examples from the main documentation
mod <- lm(mpg ~ hp, data = mtcars)
cmp <- comparisons(mod, variables = list(hp = c(90, 110)))
expect_inherits(cmp, "comparisons")


# Issue #527
mtcars$new_hp <- 49 * (mtcars$hp - min(mtcars$hp)) / (max(mtcars$hp) - min(mtcars$hp)) + 1
mod <- lm(mpg ~ log(new_hp) + factor(cyl), data = mtcars)
fdiff <- \(x) data.frame(x, x + 10)
cmp1 <- comparisons(mod, variables = list(new_hp = fdiff))
cmp2 <- comparisons(mod, variables = list(new_hp = 10))
expect_equivalent(nrow(cmp1), 32)
expect_equivalent(nrow(cmp2), 27)