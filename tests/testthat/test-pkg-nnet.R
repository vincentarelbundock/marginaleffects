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
    expect_warning(expect_marginaleffects(mod, type = "probs"))
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


test_that("predictions with multinomial outcome", {
    skip("prep for future multinom development")
    library(tidyverse)
    library(nnet)
    library(emmeans)
    library(magrittr)
    set.seed(1839)
    n <- 1200
    x <- factor(sample(letters[1:3], n, TRUE))
    y <- vector(length = n)
    y[x == "a"] <- sample(letters[4:6], sum(x == "a"), TRUE)
    y[x == "b"] <- sample(letters[4:6], sum(x == "b"), TRUE, c(1 / 4, 2 / 4, 1 / 4))
    y[x == "c"] <- sample(letters[4:6], sum(x == "c"), TRUE, c(1 / 5, 3 / 5, 2 / 5))
    dat <- tibble(x = x, y = factor(y))
    dat <- replicate(20, factor(sample(letters[7:9], n, TRUE))) %>%
      as.data.frame() %>%
      bind_cols(dat, .)
    void <- capture.output({
        m1 <- multinom(y ~ x, dat)
        m2 <- multinom(y ~ ., dat)
    })
    # class outcome not supported
    expect_error(predictions(m1, variables = "x"), regex = "type")
    expect_error(marginalmeans(m1, variables = "x"), regex = "type")
    # small predictions
    tmp <- predictions(m1, type = "probs", variables = "x")


    expect_s3_class(tmp, "data.frame")
    expect_true("predicted" %in% colnames(tmp))
    expect_equal(nrow(tmp), 9)
    tmp <- marginalmeans(m1, type = "probs", variables = "x")
    expect_s3_class(tmp, "data.frame")
    expect_equal(nrow(tmp), 3)
    # large predictions
    idx <- 3:7
    tmp <- predictions(m2, type = "probs", variables = colnames(dat)[idx])
    expect_s3_class(tmp, "data.frame")
    expect_true("predicted" %in% colnames(tmp))
    expect_equal(nrow(tmp), 3^length(idx))
    tmp <- predictions(m2, type = "probs", variables = colnames(dat)[idx])
    expect_s3_class(tmp, "data.frame")
    expect_equal(nrow(tmp), 3^length(idx))
})
