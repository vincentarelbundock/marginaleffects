source("helpers.R")
using("marginaleffects")
requiet("tinysnapshot")
using("tinysnapshot")

# from marginaleffects objects
mod <- glm(am ~ hp + wt, data = mtcars)

expect_error(plot(predictions(mod)), pattern = "plot_predictions")
expect_error(plot(slopes(mod)), pattern = "plot_slopes")
expect_error(plot(comparisons(mod)), pattern = "plot_comparisons")
