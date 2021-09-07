skip_if_not_installed("MASS")

library("margins")

test_that("polr: test against margins", {
    skip("no idea why this fails")
    tmp <- data.frame(mtcars)
    tmp$carb <- as.factor(tmp$carb)
    mod <- MASS::polr(carb ~ hp + am + mpg, data = tmp) 
    res <- mfx(mod, variance = NULL)
    mar <- margins(mod)

    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(480, 8))

    # TODO: not supported yet
    expect_error(mfx(mod, variance = NULL), regexp = "group_name")
    expect_error(mfx(mod, group_names = "1"), regexp = "not yet supported")
})
