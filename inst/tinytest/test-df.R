source("helpers.R")
requiet("emmeans")
requiet("marginaleffects")
using("marginaleffects")

# TODO: rename dat to df to make sure there's no clash with the internal keyword

dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.factor(dat$am)
mod <- lm(mpg ~ cyl, data = dat)

em <- emmeans(mod, ~cyl)
em <- confint(pairs(em), adjust = "none") |>
    dplyr::arrange(contrast)


cmp29 <- comparisons(mod, df = insight::get_df(mod))
cmpInf <- comparisons(mod)
expect_true(all(cmp29$p.value > cmpInf$p.value))
expect_true(all(cmp29$conf.low < cmpInf$conf.low))


mfx29 <- slopes(mod, df = insight::get_df(mod))
mfxInf <- slopes(mod)
expect_true(all(mfx29$p.value > mfxInf$p.value))
expect_true(all(mfx29$conf.low < mfxInf$conf.low))


# Issue #594
pre29 <- predictions(mod, df = 29)
preInf <- predictions(mod)
expect_true(all(pre29$p.value > preInf$p.value))
expect_true(all(pre29$conf.low < preInf$conf.low))

# Issue #754: allow df vector
mod <- lm(mpg ~ hp, mtcars)
a <- predictions(mod, df = 1:32)
b <- predictions(mod, df = 1)
expect_equal(sum(a$p.value == b$p.value), 1)


# Issue #627: print t instead of z in column names
if (!requiet("tinysnapshot")) exit_file("tinysnapshot")
using("tinysnapshot")


mod <- lm(mpg ~ hp, mtcars)
expect_snapshot_print(avg_comparisons(mod), "df-z")
expect_snapshot_print(avg_comparisons(mod, df = 30), "df-t")




rm(list = ls())
