skip_if_not_installed("nnet")
skip_if_not_installed("margins")
requiet("margins")


test_that("multinom basic", {
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    void <- capture.output(mod <- 
        nnet::multinom(factor(y) ~ x1 + x2, data = dat, quiet = true))
    expect_mfx(mod, type = "probs")
})

test_that("set_coef", {
    tmp <- mtcars
    tmp$cyl <- as.factor(tmp$cyl)
    void <- capture.output( old <- 
        nnet::multinom(cyl ~ hp + am + mpg, data = tmp, quiet = true))
    b <- rep(0, length(coef(old)))
    new <- set_coef(old, b)
    expect_true(all(coef(new) == 0))
    b <- rep(1, length(coef(new)))
    new <- set_coef(old, b)
    expect_true(all(coef(new) == 1))
})
