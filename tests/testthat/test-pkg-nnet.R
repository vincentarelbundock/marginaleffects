skip_if_not_installed("nnet")


test_that("warning: standard error mismatch", {
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    void <- capture.output(mod <- 
        nnet::multinom(factor(y) ~ x1 + x2, data = dat, quiet = true))
    expect_warning(marginaleffects(mod, type = "probs"), regexp = "do not match")
})


test_that("error: bad type", {
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    void <- capture.output(mod <- 
        nnet::multinom(factor(y) ~ x1 + x2, data = dat, quiet = true))
    expect_warning(expect_error(marginaleffects(mod), regexp = "type.*supported"))
})

   
test_that("multinom basic", {
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    void <- capture.output(mod <- 
        nnet::multinom(factor(y) ~ x1 + x2, data = dat, quiet = true))
    expect_warning(expect_mfx(mod, type = "probs"))
})


test_that("multinom vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$nnet_multinom_01
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    void <- capture.output(mod <- 
        nnet::multinom(factor(y) ~ x1 + x2, data = dat, quiet = true))
    mfx <- suppressWarnings(marginaleffects(mod, type = "probs"))
    mfx <- merge(tidy(mfx), stata, all = TRUE)
    # standard errors don't match
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
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


test_that("bugfix: nnet single row predictions", {
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    void <- capture.output(mod <- 
        nnet::multinom(factor(y) ~ x1 + x2, data = dat, quiet = true))
    expect_warning(marginaleffects(mod, newdata = typical(), type = "probs"))
    mfx <- suppressWarnings(marginaleffects(mod, newdata = typical(), type = "probs"))
    expect_s3_class(mfx, "data.frame")
    expect_equal(nrow(mfx), 6)
})
