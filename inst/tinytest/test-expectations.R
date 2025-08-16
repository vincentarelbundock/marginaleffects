source("helpers.R")
using("marginaleffects")

mod <- lm(mpg ~ wt + factor(am), data = mtcars)
expect_slopes(mod)
expect_predictions(mod)
expect_hypotheses(mod)
expect_comparisons(mod)
