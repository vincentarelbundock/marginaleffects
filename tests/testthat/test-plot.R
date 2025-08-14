skip_if_not_installed("tinysnapshot")

test_that("plot() gives informative errors for marginaleffects objects", {
    mod <- glm(am ~ hp + wt, data = mtcars)

    expect_error(plot(predictions(mod)), regexp = "plot_predictions")
    expect_error(plot(slopes(mod)), regexp = "plot_slopes")
    expect_error(plot(comparisons(mod)), regexp = "plot_comparisons")
})
