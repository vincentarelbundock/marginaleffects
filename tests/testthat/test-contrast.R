test_that("simple contrasts: no validity check", {
    dat <- mtcars
    dat$am <- as.logical(dat$am)
    mod <- lm(mpg ~ hp + am + factor(cyl), data = dat)
    res <- tidy(marginaleffects(mod))
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(5, 9))
})
