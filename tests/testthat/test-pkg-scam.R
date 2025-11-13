testthat::skip_if_not_installed("scam")
requiet("scam")

# Basic expectation tests
mod_simple <- scam::scam(mpg ~ s(wt, bs = "mpi"), data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# no validity
set.seed(4)
n <- 200
x1 <- runif(n) * 6 - 3
f1 <- 3 * exp(-x1^2) # unconstrained term
x2 <- runif(n) * 4 - 1
f2 <- exp(4 * x2) / (1 + exp(4 * x2)) # monotone increasing smooth
y <- f1 + f2 + rnorm(n) * .5
dat_scam <- data.frame(x1 = x1, x2 = x2, y = y)
mod <- scam(y ~ s(x1, bs = "cr") + s(x2, bs = "mpi"), data = dat_scam)
suppressWarnings(expect_slopes2(mod))
expect_predictions2(mod)
