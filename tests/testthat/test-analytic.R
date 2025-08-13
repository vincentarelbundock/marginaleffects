test_that("lm quadratic slopes", {
    set.seed(1027)
    f <- y ~ x + I(x^2)
    truth <- function(x) 1 + 2 * x
    N <- 100000
    dat <- data.frame(x = rnorm(N))
    dat$y <- 1 + 1 * dat$x + 1 * dat$x^2 + rnorm(N)
    mod <- lm(f, dat)
    nd <- datagrid(newdata = dat, x = c(-2:2))
    res <- slopes(mod, newdata = nd)
    res$truth <- truth(res$x)
    expect_equal(res$estimate, res$truth, tolerance = .01)
})


test_that("lm log slopes", {
    set.seed(30)
    f <- y ~ log(x)
    truth <- function(x) 1 / x
    N <- 10000
    dat <- data.frame(x = runif(N))
    dat$y <- log(dat$x) + rnorm(N)
    mod <- lm(f, dat)
    nd <- datagrid(newdata = dat, x = c(1:4))
    res <- slopes(mod, newdata = nd)
    res$truth <- truth(res$x)
    expect_equal(res$estimate, res$truth, tolerance = .01)
})


test_that("logit slopes", {
    set.seed(2000)
    f <- y ~ x
    beta0 <- 1
    beta1 <- .2
    truth <- function(x) beta1 * dlogis(beta0 + beta1 * x)
    N <- 1e5
    dat <- data.frame(x = rnorm(N, sd = 3))
    dat$y <- rbinom(N, 1, pr = plogis(beta0 + beta1 * dat$x))
    mod <- glm(f, data = dat, family = binomial)
    nd <- datagrid(newdata = dat, x = c(-10:10))
    res <- slopes(mod, newdata = nd)
    res$truth <- truth(res$x)
    expect_equal(res$estimate, res$truth, tolerance = .01)
})


test_that("Golder Interaction Case 1a/1b: dy/dx and variance", {
    # tests based on formulae from Matt Golder's OLS examples, for numerical accuracy and precision
    set.seed(1)
    n <- 25L
    d <- data.frame(w = rnorm(n), x = rnorm(n), z = rnorm(n))
    d[["y"]] <- with(d, w + x + z + w * x + w * z + x * z * w * x * z + rnorm(n))

    # set comparison tolerance
    tol <- 0.001
    tol_se <- 0.005

    f1.1 <- y ~ x + z + x:z
    m <- lm(f1.1, data = d)
    marg <- slopes(m)

    # ME with respect to x
    dydx <- coef(m)["x"] + (d$z * coef(m)["x:z"])
    sedydx <- sqrt(vcov(m)["x", "x"] + (d$z^2 * vcov(m)["x:z", "x:z"]) + (2 * d$z * vcov(m)["x", "x:z"]))
    expect_equal(as.numeric(marg$estimate[marg$term == "x"]), dydx, tolerance = tol)
    expect_equal(
        sedydx,
        as.numeric(marg$std.error[marg$term == "x"]),
        tolerance = tol_se
    )

    # ME with respect to z
    dydz <- coef(m)["z"] + (d$x * coef(m)["x:z"])
    sedydz <- sqrt(vcov(m)["z", "z"] + (d$x^2 * vcov(m)["x:z", "x:z"]) + (2 * d$x * vcov(m)["z", "x:z"]))
    expect_equal(as.numeric(marg$estimate[marg$term == "z"]), dydz, tolerance = tol)
    expect_equal(
        sedydz,
        as.numeric(marg$std.error[marg$term == "z"]),
        tolerance = tol_se
    )
})


test_that("Golder Interaction Case 2: dy/dx and variance", {
    set.seed(1)
    n <- 25L
    d <- data.frame(w = rnorm(n), x = rnorm(n), z = rnorm(n))
    d[["y"]] <- with(d, w + x + z + w * x + w * z + x * z * w * x * z + rnorm(n))

    tol <- 0.001
    tol_se <- 0.005

    f1.2 <- y ~ x + z + w + x:z + z:w
    m <- lm(f1.2, data = d)
    marg <- slopes(m)
    dydx <- coef(m)["x"] + (d$z * coef(m)["x:z"])
    sedydx <- sqrt(vcov(m)["x", "x"] + (d$z^2 * vcov(m)["x:z", "x:z"]) + (2 * d$z * vcov(m)["x", "x:z"]))
    expect_equal(as.numeric(marg$estimate[marg$term == "x"]), dydx, tolerance = tol)
    expect_equal(
        sedydx,
        as.numeric(marg$std.error[marg$term == "x"]),
        tolerance = tol_se
    )
})


test_that("Golder Interaction Case 3: dy/dx and variance", {
    set.seed(1)
    n <- 25L
    d <- data.frame(w = rnorm(n), x = rnorm(n), z = rnorm(n))
    d[["y"]] <- with(d, w + x + z + w * x + w * z + x * z * w * x * z + rnorm(n))

    tol <- 0.001
    tol_se <- 0.005

    f1.3 <- y ~ x + z + w + x:z + x:w + z:w
    m <- lm(f1.3, data = d)
    marg <- slopes(m)
    dydx <- coef(m)["x"] + (d$z * coef(m)["x:z"]) + (d$w * coef(m)["x:w"])
    sedydx <- sqrt(
        vcov(m)["x", "x"] +
            (d$z^2 * vcov(m)["x:z", "x:z"]) +
            (d$w^2 * vcov(m)["x:w", "x:w"]) +
            (2 * d$z * vcov(m)["x", "x:z"]) +
            (2 * d$w * vcov(m)["x", "x:w"]) +
            (2 * d$z * d$w * vcov(m)["x:z", "x:w"])
    )
    expect_equal(as.numeric(marg$estimate[marg$term == "x"]), dydx, tolerance = tol)
    expect_equal(
        sedydx,
        as.numeric(marg$std.error[marg$term == "x"]),
        tolerance = tol_se
    )
})


test_that("Golder Interaction Case 4: dy/dx and variance", {
    set.seed(1)
    n <- 25L
    d <- data.frame(w = rnorm(n), x = rnorm(n), z = rnorm(n))
    d[["y"]] <- with(d, w + x + z + w * x + w * z + x * z * w * x * z + rnorm(n))

    tol <- 0.001
    tol_se <- 0.005

    f1.4 <- y ~ x + z + w + x:z + x:w + z:w + x:z:w
    m <- lm(f1.4, data = d)
    marg <- slopes(m)
    dydx <- coef(m)["x"] + (d$z * coef(m)["x:z"]) + (d$w * coef(m)["x:w"]) + (d$z * d$w * coef(m)["x:z:w"])
    sedydx <- sqrt(
        vcov(m)["x", "x"] +
            (d$z^2 * vcov(m)["x:z", "x:z"]) +
            (d$w^2 * vcov(m)["x:w", "x:w"]) +
            (d$z^2 * d$w^2 * vcov(m)["x:z:w", "x:z:w"]) +
            (2 * d$z * vcov(m)["x", "x:z"]) +
            (2 * d$w * vcov(m)["x", "x:w"]) +
            (2 * d$z * d$w * vcov(m)["x", "x:z:w"]) +
            (2 * d$z * d$w * vcov(m)["x:z", "x:w"]) +
            (2 * d$w * d$z^2 * vcov(m)["x:z", "x:z:w"]) +
            (2 * d$z * d$w^2 * vcov(m)["x:w", "x:z:w"])
    )
    expect_equal(as.numeric(marg$estimate[marg$term == "x"]), dydx, tolerance = tol)
    expect_equal(
        sedydx,
        as.numeric(marg$std.error[marg$term == "x"]),
        tolerance = tol_se
    )
})


test_that("Golder Quadratic Case 1: dy/dx and variance", {
    set.seed(1)
    n <- 25L
    d <- data.frame(w = rnorm(n), x = rnorm(n), z = rnorm(n))
    d[["y"]] <- with(d, w + x + z + w * x + w * z + x * z * w * x * z + rnorm(n))

    tol <- 0.001
    tol_se <- 0.005

    f2.1 <- y ~ x + I(x^2)
    m <- lm(f2.1, data = d)
    marg <- slopes(m)
    dydx <- coef(m)["x"] + (2 * coef(m)["I(x^2)"] * d$x)
    sedydx <- sqrt(vcov(m)["x", "x"] + (4 * d$x^2 * vcov(m)["I(x^2)", "I(x^2)"]) + (4 * d$x * vcov(m)["x", "I(x^2)"]))
    expect_equal(as.numeric(marg$estimate[marg$term == "x"]), dydx, tolerance = tol)
    expect_equal(
        sedydx,
        as.numeric(marg$std.error[marg$term == "x"]),
        tolerance = tol_se
    )
})


test_that("Golder Quadratic Case 2: dy/dx and variance", {
    set.seed(1)
    n <- 25L
    d <- data.frame(w = rnorm(n), x = rnorm(n), z = rnorm(n))
    d[["y"]] <- with(d, w + x + z + w * x + w * z + x * z * w * x * z + rnorm(n))

    tol <- 0.001
    tol_se <- 0.005

    f2.2 <- y ~ x + I(x^2) + z
    m <- lm(f2.2, data = d)
    marg <- slopes(m)
    dydx <- coef(m)["x"] + (2 * coef(m)["I(x^2)"] * d$x)
    sedydx <- sqrt(vcov(m)["x", "x"] + (4 * d$x^2 * vcov(m)["I(x^2)", "I(x^2)"]) + (4 * d$x * vcov(m)["x", "I(x^2)"]))
    expect_equal(as.numeric(marg$estimate[marg$term == "x"]), dydx, tolerance = tol)
    expect_equal(
        sedydx,
        as.numeric(marg$std.error[marg$term == "x"]),
        tolerance = tol_se
    )
})


test_that("Golder Quadratic Case 3a/3b: dy/dx, dy/dz and variances", {
    set.seed(1)
    n <- 25L
    d <- data.frame(w = rnorm(n), x = rnorm(n), z = rnorm(n))
    d[["y"]] <- with(d, w + x + z + w * x + w * z + x * z * w * x * z + rnorm(n))

    tol <- 0.001
    tol_se <- 0.005

    f2.3 <- y ~ x + I(x^2) + z + x:z
    m <- lm(f2.3, data = d)
    marg <- slopes(m)

    # ME with respect to x
    dydx <- coef(m)["x"] + (2 * coef(m)["I(x^2)"] * d$x) + (d$z * coef(m)["x:z"])
    sedydx <- sqrt(
        vcov(m)["x", "x"] +
            (4 * d$x^2 * vcov(m)["I(x^2)", "I(x^2)"]) +
            (d$z^2 * vcov(m)["x:z", "x:z"]) +
            (4 * d$x * vcov(m)["x", "I(x^2)"]) +
            (2 * d$z * vcov(m)["x", "x:z"]) +
            (4 * d$x * d$z * vcov(m)["I(x^2)", "x:z"])
    )
    expect_equal(as.numeric(marg$estimate[marg$term == "x"]), dydx, tolerance = tol)
    expect_equal(
        sedydx,
        as.numeric(marg$std.error[marg$term == "x"]),
        tolerance = tol_se
    )

    # ME with respect to z
    dydz <- coef(m)["z"] + (d$x * coef(m)["x:z"])
    sedydz <- sqrt(vcov(m)["z", "z"] + (d$x^2 * vcov(m)["x:z", "x:z"]) + (2 * d$x * vcov(m)["z", "x:z"]))
    expect_equal(as.numeric(marg$estimate[marg$term == "z"]), dydz, tolerance = tol)
    expect_equal(
        sedydz,
        as.numeric(marg$std.error[marg$term == "z"]),
        tolerance = tol_se
    )
})


test_that("Golder Quadratic Case 4a/4b: dy/dx, dy/dz and variances", {
    set.seed(1)
    n <- 25L
    d <- data.frame(w = rnorm(n), x = rnorm(n), z = rnorm(n))
    d[["y"]] <- with(d, w + x + z + w * x + w * z + x * z * w * x * z + rnorm(n))

    tol <- 0.001
    tol_se <- 0.005

    f2.4 <- y ~ x + I(x^2) + z + x:z + I(x^2):z
    m <- lm(f2.4, data = d)
    marg <- slopes(m)

    # ME with respect to x
    dydx <- coef(m)["x"] +
        (2 * coef(m)["I(x^2)"] * d$x) +
        (d$z * coef(m)["x:z"]) +
        (2 * d$x * d$z * coef(m)["I(x^2):z"])
    sedydx <- sqrt(
        vcov(m)["x", "x"] +
            (4 * d$x^2 * vcov(m)["I(x^2)", "I(x^2)"]) +
            (d$z^2 * vcov(m)["x:z", "x:z"]) +
            (4 * (d$x^2) * (d$z^2) * vcov(m)["I(x^2):z", "I(x^2):z"]) +
            (4 * d$x * vcov(m)["x", "I(x^2)"]) +
            (2 * d$z * vcov(m)["x", "x:z"]) +
            (4 * d$x * d$z * vcov(m)["I(x^2)", "x:z"]) +
            (4 * d$x * d$z * vcov(m)["x", "I(x^2):z"]) +
            (8 * (d$x^2) * d$z * vcov(m)["I(x^2)", "I(x^2):z"]) +
            (4 * d$x * (d$z^2) * vcov(m)["x:z", "I(x^2):z"])
    )
    expect_equal(as.numeric(marg$estimate[marg$term == "x"]), dydx, tolerance = tol)
    expect_equal(
        sedydx,
        as.numeric(marg$std.error[marg$term == "x"]),
        tolerance = tol_se
    )

    # ME with respect to z
    dydz <- coef(m)["z"] + (d$x * coef(m)["x:z"]) + (d$x^2 * coef(m)["I(x^2):z"])
    sedydz <- sqrt(
        vcov(m)["z", "z"] +
            (d$x^2 * vcov(m)["x:z", "x:z"]) +
            (d$x^4 * vcov(m)["I(x^2):z", "I(x^2):z"]) +
            (2 * d$x * vcov(m)["z", "x:z"]) +
            (2 * (d$x^2) * vcov(m)["z", "I(x^2):z"]) +
            (2 * (d$x^3) * vcov(m)["x:z", "I(x^2):z"])
    )
    expect_equal(as.numeric(marg$estimate[marg$term == "z"]), dydz, tolerance = tol)
    expect_equal(
        sedydz,
        as.numeric(marg$std.error[marg$term == "z"]),
        tolerance = tol_se
    )
})
