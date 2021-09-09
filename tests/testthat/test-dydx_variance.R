test_that("marginal effects at the 'typical()' value when variance=FALSE", {
    mod <- lm(hp ~ mpg + drat, data = mtcars)
    res <- tidy(marginaleffects(mod, variance = FALSE))
    expect_s3_class(res, "data.frame")
    expect_true(all(res$std.error > 0))
})
