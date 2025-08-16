source("helpers.R")
using("marginaleffects")
requiet("biglm")

N <- 1e4
x1 <- rnorm(N)
x2 <- rnorm(N)
y <- rbinom(size = 1, n = N, prob = plogis(x1 + x2))
dat <- data.frame(y, x1, x2)

big <- bigglm(y ~ x1 + x2, data = dat, family = binomial())
small <- glm(y ~ x1 + x2, data = dat, family = binomial())

# vcov not supported
options(marginaleffects_safe = TRUE)
expect_warning(comparisons(big), pattern = "not supported")
options(marginaleffects_safe = FALSE)

# dydx supported
big_m <- comparisons(big, vcov = FALSE)
small_m <- comparisons(small, vcov = FALSE)
expect_equivalent(big_m$estimate, small_m$estimate)

big_m <- slopes(big, type = "link", vcov = FALSE)
small_m <- slopes(small, type = "link", vcov = FALSE)
t1 <- tidy(big_m)
t2 <- tidy(small_m)
expect_equivalent(t1$estimate, t2$estimate)
