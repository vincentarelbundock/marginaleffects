
test_that("factor before fitting or in formula is the same", {
    tmp <- mtcars
    tmp$cyl <- factor(tmp$cyl)
    mod1 <- lm(mpg ~ hp + factor(cyl), mtcars)
    mod2 <- lm(mpg ~ hp + cyl, tmp)
    mfx1 <- suppressWarnings(marginaleffects(mod1))
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

test_that("smart detect factor() in formula", {
    requiet("estimatr")
    model <- lm_robust(carb ~ wt + factor(cyl), se_type = "stata", data = mtcars)
    k <- marginaleffects(model)
    expect_true(all(c("", "8 - 4") %in% k$contrast))
})

test_that("factor in formula with incomplete newdata", {
    mod <- lm(mpg ~ factor(cyl), data = mtcars)
    mfx1 <- marginaleffects(mod, newdata = data.frame(cyl = 4))
    mfx2 <- marginaleffects(mod, newdata = datagrid(cyl = 4))
    expect_equal(mfx1[, 1:6], mfx2[, 1:6], ignore_attr = TRUE)
})


test_that("bugs stay dead: strata in coxph", {
    requiet("survival")
    skip("works interactively")
    skip_if_not_installed("insight", minimum_version = "0.16.0.7") # bug in get_data.coxph()
    test1 <- data.frame(time = c(4,3,1,1,2,2,3),
                        status = c(1,1,1,0,1,1,0),
                        x = c(0,2,1,1,1,0,0),
                        sex = c(0,0,0,0,1,1,1))
    mod <- coxph(Surv(time, status) ~ x + strata(sex),
                 data = test1,
                 ties = "breslow")
    mfx <- marginaleffects(mod, variables = "x", newdata = datagrid(sex = 0), type = "lp")
    expect_s3_class(mfx, "marginaleffects")
})
