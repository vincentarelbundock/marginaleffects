source("helpers.R")
mod <- lm(mpg ~ qsec + hp * factor(cyl), data = mtcars)

pre1 <- avg_predictions(mod, by = "cyl")
pre2 <- avg_predictions(mod, by = "cyl", p_adjust = "hochberg")
expect_true(any(pre1$p.value < pre2$p.value))
expect_true(all(pre1$p.value <= pre2$p.value))
expect_false("conf.low" %in% colnames(pre2))

cmp1 <- avg_comparisons(mod, variables = list(cyl = "pairwise"))
cmp2 <- avg_comparisons(mod, variables = list(cyl = "pairwise"), p_adjust = "hochberg")
expect_true(any(cmp1$p.value < cmp2$p.value))
expect_true(all(cmp1$p.value <= cmp2$p.value))
expect_false("conf.low" %in% colnames(cmp2))

mfx1 <- avg_slopes(mod)
mfx2 <- avg_slopes(mod, p_adjust = "hochberg")
expect_true(any(mfx1$p.value < mfx2$p.value))
expect_true(all(mfx1$p.value <= mfx2$p.value))
expect_false("conf.low" %in% colnames(mfx2))

mm1 <- marginal_means(mod)
mm2 <- marginal_means(mod, p_adjust = "hochberg")
expect_true(any(mm1$p.value < mm2$p.value))
expect_true(all(mm1$p.value <= mm2$p.value))
expect_false("conf.low" %in% colnames(mm2))

expect_error(marginal_means(mod, p_adjust = "junk"), pattern = "Assertion")