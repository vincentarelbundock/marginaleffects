test_that("counterfactual(): factor, logical, automatic variable", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    tmp$gear <- as.factor(tmp$gear)
    mod <- lm(mpg ~ hp * wt + am + gear, data = tmp)
    res <- counterfactual(mod,
                          at = list(hp = c(100, 110),
                                    gear = c(3, 4),
                                    am = TRUE))
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(128, 5))
})

test_that("typical(): factor, logical, numeric", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    tmp$gear <- as.factor(tmp$gear)
    res <- typical(data = tmp)
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(1, 11))
    expect_equal(sum(sapply(res, is.logical)), 1)
    expect_equal(sum(sapply(res, is.factor)), 1)
    expect_equal(sum(sapply(res, is.numeric)), 9)
})


test_that("typical number of rows", {
    mod <- lm(mpg ~ hp * wt, data = mtcars)
    nd <- typical(mod, at = list(hp = c(100, 110)))
    expect_equal(dim(marginaleffects(mod, newdata = nd)), c(4, 6))
})
