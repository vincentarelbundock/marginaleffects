skip_if(getRversion() < 4.0) # problem on github actions with oldrel
requiet("nnet")

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
    expect_warning(expect_error(marginaleffects(mod), regexp = "must be an element"))
})
   
test_that("multinom basic", {
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    void <- capture.output(mod <- 
        nnet::multinom(factor(y) ~ x1 + x2, data = dat, quiet = true))
    expect_warning(expect_marginaleffects(mod, type = "probs"))
})

test_that("marginaleffects summary", {
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    void <- capture.output(mod <- 
        nnet::multinom(factor(y) ~ x1 + x2, data = dat, quiet = true))
    mfx <- suppressWarnings(marginaleffects(mod, type = "probs"))
    s <- tidy(mfx)
    expect_false(anyNA(s$estimate))
    expect_false(anyNA(s$std.error))
})

test_that("multinom vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$nnet_multinom_01
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    dat$y <- as.factor(dat$y)
    void <- capture.output(mod <- 
        nnet::multinom(y ~ x1 + x2, data = dat, quiet = true))
    mfx <- suppressWarnings(marginaleffects(mod, type = "probs"))
    mfx <- merge(tidy(mfx), stata, all = TRUE)
    # standard errors don't match
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    # expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)
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
    expect_warning(marginaleffects(mod, newdata = datagrid(), type = "probs"))
    mfx <- suppressWarnings(marginaleffects(mod, newdata = datagrid(), type = "probs"))
    expect_s3_class(mfx, "data.frame")
    expect_equal(nrow(mfx), 6)
})

test_that("predictions with multinomial outcome", {
    skip_if_not_installed("insight", minimum_version = "0.14.4.1")

    set.seed(1839)
    n <- 1200
    x <- factor(sample(letters[1:3], n, TRUE))
    y <- vector(length = n)
    y[x == "a"] <- sample(letters[4:6], sum(x == "a"), TRUE)
    y[x == "b"] <- sample(letters[4:6], sum(x == "b"), TRUE, c(1 / 4, 2 / 4, 1 / 4))
    y[x == "c"] <- sample(letters[4:6], sum(x == "c"), TRUE, c(1 / 5, 3 / 5, 2 / 5))
    dat <- data.frame(x = x, y = factor(y))
    tmp <- as.data.frame(replicate(20, factor(sample(letters[7:9], n, TRUE))))
    dat <- cbind(dat, tmp)
    void <- capture.output({
        m1 <- multinom(y ~ x, dat)
        m2 <- multinom(y ~ ., dat)
    })

    # class outcome not supported
    expect_error(predictions(m1, variables = "x"), regex = "type")
    expect_error(marginalmeans(m1, variables = "x"), regex = "type")

    # small predictions
    pred1 <- predictions(m1, type = "probs")
    pred2 <- predictions(m1, type = "probs", variables = "x")
    expect_predictions(pred1, n_row = nrow(dat) * 3)
    expect_predictions(pred2, n_row = 9)

    # large predictions
    idx <- 3:7
    pred <- predictions(m2, type = "probs", variables = colnames(dat)[idx])
    expect_predictions(pred, n_row = 729)

    # massive prediction raises error
    expect_error(predictions(m2, type = "probs", variables = colnames(dat)[3:ncol(dat)]),
                 regexp = "1 billion rows")
})


test_that("bugs stay dead #218", {
    set.seed(42)
    dat <- data.frame(y = factor(sample(c(rep(4, 29), rep(3, 15), rep(2, 4), rep(1, 2)))),
                      x = factor(sample(c(rep(1, 17), rep(2, 12), rep(2, 12), rep(1, 9)))),
                      z1 = sample(1:2, 50, replace=TRUE), z2=runif(50, 16, 18))
    void <- capture.output(
        model <- nnet::multinom(y ~ x + z1 + z2, data = dat, verbose = FALSE, hessian = TRUE))
    mfx <- suppressWarnings(marginaleffects(model, type = "probs"))
    expect_s3_class(mfx, "marginaleffects")
})
