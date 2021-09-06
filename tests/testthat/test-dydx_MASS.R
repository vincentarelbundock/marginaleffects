skip_if_not_installed("MASS")

test_that("polr", {
    library(MASS)
    tmp <- mtcars
    tmp$carb <- as.factor(tmp$carb)
    mod <- polr(carb ~ hp + am + mpg, data = tmp) 
    res <- mfx(mod, group_names = c("1", "2", "3", "4", "6"), variance = NULL)
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(32, 19))
    # TODO: not supported yet
    expect_error(mfx(mod, variance = NULL), regexp = "must specify")
    expect_error(mfx(mod, group_names = "1"), regexp = "not supported")
})
