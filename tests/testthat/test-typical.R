skip_if_not_installed("fixest")

test_that("all manual", {
    mod <- lm(hp ~ mpg, mtcars)
    nd <- typical(model = mod, mpg = 110)
    expect_s3_class(nd, "data.frame")
    expect_equal(dim(nd), c(1, 1))
})


test_that("errors and warnings", {
    mod <- lm(hp ~ mpg, mtcars)
    expect_error(typical(), regexp = "should not both")
    expect_error(typical(model = mod, data = mtcars), regexp = "must be")

    mod <- lm(hp ~ factor(cyl), mtcars)
    expect_error(typical(model = mod, cyl = "4"), NA)
    expect_error(typical(model = mod, cyl = "2"), regexp = "must be one of the factor levels")

    mod <- fixest::feols(mpg ~ hp | cyl, data = mtcars)
    expect_warning(typical(model = mod), regexp = "cluster")
})
