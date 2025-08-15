test_that("multcomp functionality works correctly", {
    mod <- lm(mpg ~ qsec + hp * factor(cyl), data = mtcars)

    # Issue 1381: don't know how to set different null in multcomp::glht()
    expect_error(
        hypotheses(mod, hypothesis = -10, multcomp = "bonferroni"),
        pattern = "multcomp.*number"
    )

    cmp1 <- avg_comparisons(mod, variables = list(cyl = "pairwise"))
    cmp2 <- hypotheses(cmp1, multcomp = "hochberg")
    expect_true(any(cmp1$p.value < cmp2$p.value))
    expect_true(all(round(cmp1$p.value, 4) <= round(cmp2$p.value, 4)))
    expect_true(sum(round(cmp1$p.value, 4) < round(cmp2$p.value, 4)) > 1)

    mfx1 <- avg_slopes(mod)
    mfx2 <- hypotheses(mfx1, multcomp = "hochberg")
    mfx1$p.value <- round(mfx1$p.value, 4)
    mfx2$p.value <- round(mfx2$p.value, 4)
    expect_true(any(mfx1$p.value < mfx2$p.value))
    expect_true(all(mfx1$p.value <= mfx2$p.value))
    expect_true(sum(round(mfx1$p.value, 4) < round(mfx2$p.value, 4)) > 1)

    set.seed(48103)
    x <- rnorm(10)
    y <- rnorm(10)
    z <- sample(c("a", "b", "c"), 10, replace = TRUE)
    mod <- lm(y ~ x * z)
    pre1 <- avg_predictions(mod, by = "z")
    pre2 <- hypotheses(pre1, multcomp = "single-step")
    expect_true(any(pre1$p.value < pre2$p.value))
    expect_true(all(pre1$p.value <= pre2$p.value))

    mod <- lm(y ~ x * z)
    hyp1 <- hypotheses(mod)
    hyp2 <- hypotheses(mod, multcomp = "single-step")
    expect_true(any(hyp1$p.value < hyp2$p.value))
    expect_true(all(hyp1$p.value <= hyp2$p.value))

    # Issue #1414
    mod <- lm(mpg ~ factor(gear), data = mtcars)
    preds <- avg_predictions(mod, variables = "gear", df = 29)
    h1 <- hypotheses(preds, df = 29, hypothesis = ~pairwise, multcomp = "bonferroni")
    h2 <- hypotheses(preds, hypothesis = ~pairwise, multcomp = "bonferroni")
    expect_true(all(h1$p.value > h2$p.value))
    expect_true(all(h1$conf.low < h2$conf.low))
    expect_true(all(h1$conf.high > h2$conf.high))
    expect_true(all(h1$s.value < h2$s.value))
})
