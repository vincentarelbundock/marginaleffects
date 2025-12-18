testthat::skip_if_not_installed("biglm")
requiet("biglm")

# Basic expectation tests
mod_simple <- biglm::biglm(mpg ~ wt + am, data = mtcars)
# expect_warning(avg_slopes(mod_simple), regexp = "vcov.*not supported")

expect_slopes2(mod_simple, se = FALSE)
expect_predictions2(mod_simple, se = FALSE)
expect_hypotheses2(mod_simple, se = FALSE)
expect_comparisons2(mod_simple, se = FALSE)

N <- 1e4
x1 <- rnorm(N)
x2 <- rnorm(N)
y <- rbinom(size = 1, n = N, prob = plogis(x1 + x2))
dat_biglm <- data.frame(y, x1, x2)

big <- bigglm(y ~ x1 + x2, data = dat_biglm, family = binomial())
small <- glm(y ~ x1 + x2, data = dat_biglm, family = binomial())

# vcov not supported
options(marginaleffects_safe = TRUE)
expect_warning(comparisons(big), regexp = "not supported")
options(marginaleffects_safe = FALSE)

# dydx supported
big_m <- comparisons(big, vcov = FALSE)
small_m <- comparisons(small, vcov = FALSE)
expect_equal(big_m$estimate, small_m$estimate, ignore_attr = TRUE)

big_m <- slopes(big, type = "link", vcov = FALSE)
small_m <- slopes(small, type = "link", vcov = FALSE)
t1 <- tidy(big_m)
t2 <- tidy(small_m)
expect_equal(t1$estimate, t2$estimate, ignore_attr = TRUE)
