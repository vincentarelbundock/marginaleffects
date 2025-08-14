skip_if_not_installed("robust")
withr_library("robust")

test_that("robust package works", {
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    mod <- lmRob(mpg ~ hp + cyl, data = mtcars)
    expect_s3_class(slopes(mod), "slopes")
    expect_s3_class(predictions(mod), "predictions")
})

