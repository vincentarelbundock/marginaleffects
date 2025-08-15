test_that("MCMCglmm package works", {
    skip_if_not_installed("MCMCglmm")
    skip_on_cran() # EXPENSIVE test

    withr_library("MCMCglmm")

    mod <- MCMCglmm(mpg ~ hp, random = ~carb, data = mtcars, verbose = FALSE)

    p <- avg_comparisons(mod, newdata = mtcars)
    expect_s3_class(p, "comparisons")
    expect_equal(nrow(p), 1, ignore_attr = TRUE)

    p <- avg_predictions(mod, by = "carb", newdata = mtcars)
    expect_s3_class(p, "predictions")
    expect_equal(nrow(p), 6, ignore_attr = TRUE)
})
