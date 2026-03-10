source("helpers.R")

# in 0.30.1 this took forever because we didn't prune newdata
options(marginaleffects_safe = TRUE)
dat <- data.frame(matrix(rnorm(101 * 100), ncol = 101))
mod <- lm(X1 ~ X2 + X3, data = dat)
expect_warning(avg_slopes(mod), pattern = "102 columns")
expect_slopes(mod)
expect_comparisons(mod)
expect_predictions(mod)
options(marginaleffects_safe = FALSE)
