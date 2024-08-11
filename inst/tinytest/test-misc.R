source("helpers.R")

# important for modelsummary glance
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am + factor(cyl), tmp)
expect_inherits(attr(predictions(mod), "model"), "lm")
expect_inherits(attr(comparisons(mod), "model"), "lm")
expect_inherits(attr(avg_slopes(mod), "model"), "lm")

# Issue #1089 white space in variable name
tmp <- mtcars
colnames(tmp)[1] <- "Miles per gallon"
mod <- lm(hp ~ wt * `Miles per gallon`, tmp)
s <- avg_slopes(mod)
expect_inherits(s, "slopes")
expect_equal(nrow(s), 2)
s <- avg_slopes(mod, variables = "Miles per gallon")
expect_inherits(s, "slopes")
expect_equal(nrow(s), 1)
