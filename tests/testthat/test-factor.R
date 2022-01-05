
test_that("factor before fitting or in formula is the same", {
    tmp <- mtcars
    tmp$cyl <- factor(tmp$cyl)
    mod1 <- lm(mpg ~ hp + factor(cyl), mtcars)
    mod2 <- lm(mpg ~ hp + cyl, tmp)
    mfx1 <- marginaleffects(mod1)
    mfx2 <- marginaleffects(mod2)
    expect_equal(mfx1$estimate, mfx2$estimate)
    expect_equal(mfx1$std.error, mfx2$std.error)
})

test_that("factor on LHS and RHS at the same time.", {
    data(housing, package = "MASS")
    mod <- MASS::polr(Infl ~ Sat + Freq, data = housing)
    mfx <- marginaleffects(mod, type = "probs")
    expect_s3_class(mfx, "marginaleffects")
    expect_true(all(c("Low", "Medium", "High") %in% mfx$group))
})

test_that("warn when there are no factors in newdata but there is a factor tranform in the term labels", {
    skip_if_not_installed("estimatr")
    requiet("estimatr")
    model <- lm_robust(carb ~ wt + factor(cyl), se_type = "stata", data = mtcars)
    k <- marginaleffects(model)
    expect_warning(
        expect_error(marginaleffects(model, newdata = mtcars),
                     regexp = "new levels"),
        regexp = "before fitting")
})

test_that("bugs stay dead: factor in survreg", {
    skip("https://github.com/vincentarelbundock/marginaleffects/issues/160")
    requiet("survival")
    stata <- readRDS(test_path("stata/stata.rds"))$survival_coxph_01
    test1 <- list(time = c(4,3,1,1,2,2,3),
                  status = c(1,1,1,0,1,1,0),
                  x = c(0,2,1,1,1,0,0),
                  sex = c(0,0,0,0,1,1,1))
    mod <- coxph(Surv(time, status) ~ x + strata(sex),
                 data = test1,
                 ties = "breslow")
    mfx <- marginaleffects(mod, variables = "x", newdata = datagrid(sex = 0), type = "lp")
})
