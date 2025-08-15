skip_if_not_installed("biglm")

test_that("biglm package works", {
    withr_library("biglm")

    N <- 1e4
    x1 <- rnorm(N)
    x2 <- rnorm(N)
    y <- rbinom(size = 1, n = N, prob = plogis(x1 + x2))
    dat <- data.frame(y, x1, x2)

    big <- bigglm(y ~ x1 + x2, data = dat, family = binomial())
    small <- glm(y ~ x1 + x2, data = dat, family = binomial())

    # vcov not supported
    cmp <- comparisons(big)
    expect_false("std.error" %in% colnames(cmp))

    # dydx supported
    big_m <- comparisons(big, vcov = FALSE)
    small_m <- comparisons(small, vcov = FALSE)
    expect_equal(big_m$estimate, small_m$estimate, ignore_attr = TRUE)

    big_m <- slopes(big, type = "link", vcov = FALSE)
    small_m <- slopes(small, type = "link", vcov = FALSE)
    expect_equal(big_m$estimate, small_m$estimate, ignore_attr = TRUE)
})
