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
expect_equivalent(nrow(cmp2), 27)


# Issue #720
mod <- lm(mpg ~ hp * qsec, dat = mtcars)
cmp <- avg_comparisons(mod, variables = list(hp = "2sd"))
expect_equivalent(cmp$contrast, "(x + sd) - (x - sd)")



# Issue #622 cross-contrasts
mod <- lm(mpg ~ am * factor(cyl), data = mtcars)
cmp <- comparisons(mod, variables = c("cyl", "am"), cross = TRUE)
expect_equivalent(nrow(cmp), 64)
cmp <- avg_comparisons(mod, variables = c("cyl", "am"), cross = TRUE)
expect_equivalent(nrow(cmp), 2)



rm(list = ls())