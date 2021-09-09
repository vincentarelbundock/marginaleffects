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
