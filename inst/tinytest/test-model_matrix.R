source("helpers.R")
using("marginaleffects")
set.seed(1024)

mod <- lm(mpg ~ hp + factor(cyl), mtcars)
p <- avg_predictions(mod, newdata = "balanced")
M <- attr(attr(p, "newdata"), "marginaleffects_model_matrix")
expect_true(is.matrix(M))

mod <- lm(mpg ~ factor(cyl), data = mtcars)
p <- predictions(mod, by = "cyl") 
M <- attr(attr(p, "newdata"), "marginaleffects_model_matrix")
expect_true(is.matrix(M))
