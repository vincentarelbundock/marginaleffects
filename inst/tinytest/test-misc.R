source("helpers.R")

# important for modelsummary glance
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am + factor(cyl), tmp)
expect_inherits(attr(predictions(mod), "model"), "lm")
expect_inherits(attr(comparisons(mod), "model"), "lm")
expect_inherits(attr(avg_slopes(mod), "model"), "lm")