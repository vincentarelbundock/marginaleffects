source("helpers.R")
requiet("biglm")

x1 <- rnorm(1e5)
x2 <- rnorm(1e5)
y <- rbinom(size = 1, n = 1e5, prob = plogis(x1 + x2))
dat <- data.frame(y, x1, x2)

big <- bigglm(y ~ x1 + x2, data = dat, family = binomial())
small <- glm(y ~ x1 + x2, data = dat, family = binomial())

big_m <- comparisons(big, type = "response")
small_m <- comparisons(small, type = "response")
tidy(big_m)
tidy(small_m)

big_m <- marginaleffects(big, type = "link")
small_m <- marginaleffects(small, type = "link")
tidy(big_m)
tidy(small_m)

