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
fdiff <- \(x) data.frame(x, x + 10)
cmp1 <- comparisons(mod, variables = list(new_hp = fdiff))
cmp2 <- comparisons(mod, variables = list(new_hp = 10))
expect_equivalent(nrow(cmp1), 32)
expect_equivalent(nrow(cmp2), 27)
