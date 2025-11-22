# from marginaleffects objects
mod <- glm(am ~ hp + wt, data = mtcars)

expect_error(plot(predictions(mod)), regexp = "plot_predictions")
expect_error(plot(slopes(mod)), regexp = "plot_slopes")
expect_error(plot(comparisons(mod)), regexp = "plot_comparisons")
