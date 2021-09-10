test_that("lm quadratic", {
    set.seed(1027)
    f <- y ~ x + I(x^2)
    truth <- function(x) 1 + 2 * x
    N <- 100000
    dat <- data.frame(x = rnorm(N))
    dat$y <- 1 + 1 * dat$x + 1 * dat$x^2 + rnorm(N)
    mod <- lm(f, dat)
    nd <- typical(data = dat, x = c(-2:2))
    res <- marginaleffects(mod, newdata = nd)
    res$truth <- truth(res$x)
    expect_equal(res$dydx, res$truth, tolerance = .01)
})


test_that("lm log", {
    set.seed(30)                    
    f <- y ~ log(x)
    truth <- function(x) 1 / x
    N <- 10000
    dat <- data.frame(x = runif(N))
    dat$y <- log(dat$x) + rnorm(N)
    mod <- lm(f, dat)
    nd <- typical(data = dat, x = c(1:4))
    res <- marginaleffects(mod, newdata = nd)
    res$truth <- truth(res$x)
    expect_equal(res$dydx, res$truth, tolerance = .01)
})


test_that("logit", {
    set.seed(2000)
    f <- y ~ x
    beta0 <- 1
    beta1 <- .2
    truth <- function(x) beta1 * dlogis(beta0 + beta1 * x)
    N <- 1e5
    dat <- data.frame(x = rnorm(N, sd = 3))
    dat$y <- rbinom(N, 1, pr = plogis(beta0 + beta1 * dat$x))
    mod <- glm(f, data = dat, family = binomial)
    nd <- typical(data = dat, x = c(-10:10))
    res <- marginaleffects(mod, newdata = nd)
    res$truth <- truth(res$x)
    expect_equal(res$dydx, res$truth, tolerance = .01)
})
