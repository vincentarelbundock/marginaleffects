skip_if_not_installed("fixest")

library("margins")

test_that("fixest", {
    # logit is identical
    counterfactuals <- data.frame(hp = 110, wt = c(min(mtcars$wt), max(mtcars$wt)), cyl = 4)
    mod1 = fixest::feglm(am ~ hp * wt, data = mtcars, family = "binomial")
    mod2 = glm(am ~ hp * wt, data = mtcars, family = "binomial")
    expect_equal(marginsxp(mod1, newdata = counterfactuals),
                 marginsxp(mod2, newdata = counterfactuals),
                 tolerance = 1e-4)

    # TODO: this only checks if it outputs a data.frame, not if the results are correct
    mod = fixest::feglm(am ~ hp * wt | cyl, data = mtcars, family = "binomial")
    res = marginsxp(mod, newdata = counterfactuals)
    expect_s3_class(res, "data.frame")
})
