skip_if_not_installed("nnet")

library("margins")

test_that("multinom: mfx", {
    tmp <- mtcars
    tmp$cyl <- as.factor(tmp$cyl)
    void <- capture.output( mod <- 
        nnet::multinom(cyl ~ hp + am + mpg, data = tmp, quiet = true))

    res <- marginsxp(mod, 
                     variance = NULL,
                     prediction_type = "probs")
    expect_s3_class(res, "data.frame")

    # TODO: `margins` appears to break with numeric regressors but not factors
    # here it's the opposite: we don't support factors. Fix this and compare
    # results.
    expect_error(margins(mod))
})


test_that("reset_coefs", {
    tmp <- mtcars
    tmp$cyl <- as.factor(tmp$cyl)
    void <- capture.output( old <- 
        nnet::multinom(cyl ~ hp + am + mpg, data = tmp, quiet = true))
    b <- rep(0, length(coef(old)))
    new <- reset_coefs(old, b)
    expect_true(all(coef(new) == 0))
    b <- rep(1, length(coef(new)))
    new <- reset_coefs(old, b)
    expect_true(all(coef(new) == 1))
})


test_that("multinom: variance", {
    skip("Looks good but not sure this is correct.")
    N <- 100
    tmp <- data.frame(x1 = rnorm(N), x2 = rnorm(N))
    tmp$y <- tmp$x1 + tmp$x2 + rnorm(N)
    tmp$y <- cut(tmp$y, breaks = 4)
    tmp$y <- factor(as.numeric(tmp$y))
    void <- capture.output( mod <- nnet::multinom(y ~ x1 + x2, data = tmp, quiet = true))
    res <- marginsxp(mod, prediction_type = "probs")
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(800, 8))
})
