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
