source("helpers.R")
using("marginaleffects")
# https://stackoverflow.com/questions/72533745/loading-logistf-breaks-mcmcglmm
if (!EXPENSIVE) exit_file("EXPENSIVE")
requiet("MCMCglmm")

# Basic expectation tests
mod <- MCMCglmm(mpg ~ hp, random = ~carb, data = mtcars, verbose = FALSE)
expect_slopes(mod, newdata = mtcars, se = FALSE)
expect_predictions(mod, newdata = mtcars, se = FALSE)
expect_hypotheses(mod, newdata = mtcars, se = FALSE)
expect_comparisons(mod, newdata = mtcars, se = FALSE)


p <- avg_comparisons(mod, newdata = mtcars)
expect_inherits(p, "comparisons")
expect_equivalent(nrow(p), 1)

p <- avg_predictions(mod, by = "carb", newdata = mtcars)
expect_inherits(p, "predictions")
expect_equivalent(nrow(p), 6)
