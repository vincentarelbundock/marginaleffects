testthat::skip_if_not_installed("MCMCglmm")

# https://stackoverflow.com/questions/72533745/loading-logistf-breaks-mcmcglmm
testthat::skip_if(!EXPENSIVE, "EXPENSIVE")
requiet("MCMCglmm")

# Basic expectation tests
mod <- MCMCglmm(mpg ~ hp, random = ~carb, data = mtcars, verbose = FALSE)
expect_slopes2(mod, newdata = mtcars, se = FALSE)
expect_predictions2(mod, newdata = mtcars, se = FALSE)
expect_hypotheses2(mod, newdata = mtcars, se = FALSE)
expect_comparisons2(mod, newdata = mtcars, se = FALSE)


p <- avg_comparisons(mod, newdata = mtcars)
expect_s3_class(p, "comparisons")
expect_equal(nrow(p), 1, ignore_attr = TRUE)

p <- avg_predictions(mod, by = "carb", newdata = mtcars)
expect_s3_class(p, "predictions")
expect_equal(nrow(p), 6, ignore_attr = TRUE)
