testthat::skip_if_not_installed("emmeans")
requiet("emmeans")

# TODO: rename dat to df to make sure there's no clash with the internal keyword

dat_df <- mtcars
dat_df$cyl <- as.factor(dat_df$cyl)
dat_df$am <- as.factor(dat_df$am)
mod <- lm(mpg ~ cyl, data = dat_df)

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
mod <- lm(mpg ~ hp, mtcars)
expect_snapshot(avg_comparisons(mod))
expect_snapshot(avg_comparisons(mod, df = 30))


# df = "residual"
dat_df2 <- transform(mtcars, cyl = factor(cyl))
mod <- lm(mpg ~ cyl, data = dat_df2)
c1 <- avg_comparisons(mod, df = "residual")
c2 <- avg_comparisons(mod)
expect_true(all(c1$p.value > c2$p.value))
expect_true(all(c1$s.value < c2$s.value))
expect_true(all(c1$conf.low < c2$conf.low))
p1 <- predictions(mod, df = "residual")
p2 <- predictions(mod)
expect_true(all(p1$p.value > p2$p.value))
expect_true(all(p1$s.value < p2$s.value))
expect_true(all(p1$conf.low < p2$conf.low))
h1 <- hypotheses(mod, df = "residual")
h2 <- hypotheses(mod)
expect_true(all(c1$p.value > c2$p.value))
expect_true(all(c1$s.value < c2$s.value))
expect_true(all(c1$conf.low < c2$conf.low))
