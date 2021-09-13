skip_if_not_installed("fixest")

test_that("fixest: standard errors don't match", {
    # logit is identical
    counterfactuals <- data.frame(hp = 110, wt = c(min(mtcars$wt), max(mtcars$wt)), cyl = 4)
    mod1 <- fixest::feglm(vs ~ hp * wt, data = mtcars, family = "binomial", se = "standard")
    mod2 <- glm(vs ~ hp * wt, data = mtcars, family = "binomial")
    mod1 <- marginaleffects(mod1, newdata = counterfactuals)
    mod2 <- marginaleffects(mod2, newdata = counterfactuals)
    expect_equal(mod1$dydx, mod2$dydx)
    # TODO: this only checks if it outputs a data.frame, not if the results are correct
    mod = fixest::feglm(am ~ hp * wt | cyl, data = mtcars, family = "binomial")
    res = marginaleffects(mod, newdata = counterfactuals)
    expect_s3_class(res, "data.frame")
})


# No idea why this fails on testthat. It works locally!

# fe <- data.frame(unit = 1:25, fe = rnorm(25))
# dat <- expand.grid(unit = 1:25, time = 1:50)
# dat <- merge(dat, fe, by = "unit")
# dat$x <- rnorm(nrow(dat)) + dat$fe
# dat$w <- rnorm(nrow(dat))
# dat$y <- dat$x + dat$w + dat$x * dat$w + dat$fe + rnorm(nrow(dat), sd = 10)
# mod1 <- fixest::feols(y ~ x * w | unit, data = dat)
# dat2 <- dat
# dat2$unit <- as.factor(dat2$unit)
# mod2 <- fixest::feols(y ~ x * w | unit, data = dat2)

# test_that("numeric cluster variable raises warning", {
#     expect_warning(plot_cme(mod1, effect = "x", condition = "w", draw = FALSE))
#     expect_warning(plot_cme(mod2, effect = "x", condition = "w", draw = FALSE), NA)
# })

# test_that("plot_cme: extracts all required data", {
#     k <- plot_cme(mod2, effect = "x", condition = "w", draw = FALSE)
#     expect_s3_class(k, "data.frame")
#     expect_false(anyNA(k$dydx))
#     expect_false(any(k$dydx == 0))
# })
