source("helpers.R")
if (!EXPENSIVE) exit_file("EXPENSIVE")
requiet("MCMCglmm")

mod <- MCMCglmm(mpg ~ hp, random = ~carb, data = mtcars, verbose = FALSE)

p <- avg_comparisons(mod, newdata = mtcars)
expect_inherits(p, "comparisons")
expect_equivalent(nrow(p), 1)

p <- avg_predictions(mod, by = "carb", newdata = mtcars)
expect_inherits(p, "predictions")
expect_equivalent(nrow(p), 6)



rm(list = ls())