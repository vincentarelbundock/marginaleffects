source("helpers.R")
using("marginaleffects")

# conf.level argument changes conf.int size
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ hp + cyl, data = mtcars)
mfx95 <- slopes(mod, conf.level = .95)
mfx99 <- slopes(mod, conf.level = .99)
cmp95 <- comparisons(mod, conf.level = .95)
cmp99 <- comparisons(mod, conf.level = .99)
pre95 <- predictions(mod, conf.level = .95)
pre99 <- predictions(mod, conf.level = .99)
expect_true(all(mfx95$conf.low > mfx99$conf.low))
expect_true(all(mfx95$conf.high < mfx99$conf.high))
expect_true(all(cmp95$conf.low > cmp99$conf.low))
expect_true(all(cmp95$conf.high < cmp99$conf.high))
expect_true(all(pre95$conf.low > pre99$conf.low))
expect_true(all(pre95$conf.high < pre99$conf.high))


# tidy() inherits conf_level
cmp95 <- comparisons(mod)
cmp99 <- comparisons(mod, conf_level = .99)
tid95 <- tidy(cmp95)
tid99 <- tidy(cmp99)
expect_true(all(tid95$conf.low > tid99$conf.low))
expect_true(all(tid95$conf.high < tid99$conf.high))


# conf.low manual
mod <- lm(mpg ~ hp, data = mtcars)
cmp <- comparisons(mod)
critical_z <- qnorm(.025)
lb <- cmp$estimate - abs(critical_z) * cmp$std.error
ub <- cmp$estimate + abs(critical_z) * cmp$std.error
expect_equivalent(cmp$conf.low, lb)
expect_equivalent(cmp$conf.high, ub)


rm(list = ls())