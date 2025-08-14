test_that("basic model functionality works", {
    dat <- transform(mtcars, gear = factor(gear))
    mod <- lm(mpg ~ hp + gear, data = dat)
    pre <- predictions(mod)

    expect_s3_class(slopes(mod), "slopes")
    expect_s3_class(pre, "predictions")
})
