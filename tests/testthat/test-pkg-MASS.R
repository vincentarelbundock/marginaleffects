skip_if_not_installed("MASS")
skip_if_not_installed("emmeans")
skip_if_not_installed("margins")

requiet("margins")
requiet("MASS")
requiet("emmeans")


### marginaleffects

test_that("rlm: marginaleffects: vs. margins", {
    model <- MASS::rlm(mpg ~ hp + drat, mtcars)
    mfx <- marginaleffects(model)
    expect_marginaleffects(model, n_unique = 1)
    mar <- margins(model)
    expect_true(test_against_margins(mfx, mar))
})

test_that("glm.nb: marginaleffects: vs. margins", {
    # margins does not support unit-level standard errors
    model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    mfx <- marginaleffects(model)
    mar <- margins(model)
    expect_true(test_against_margins(mfx, mar))
})

test_that("glm.nb: marginaleffects: vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$mass_glm_nb
    model <- suppressWarnings(
        MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
})

test_that("polr: marginaleffects: vs. Stata", {
    skip("works interactively")
    stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    mod <- MASS::polr(factor(y) ~ x1 + x2, data = dat)
    mfx <- marginaleffects(mod, type = "probs")
    mfx <- tidy(mfx)
    mfx <- merge(mfx, stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
    expect_marginaleffects(mod, type = "probs")
})


### predictions

test_that("polr: predictions: no validity", {
    skip_if_not_installed("insight", minimum_version = "0.14.4.1")
    mod <- MASS::polr(factor(gear) ~ mpg + factor(cyl), data = mtcars)
    pred <- predictions(mod, type = "probs")
    expect_predictions(pred, se = FALSE)
})

test_that("glm.nb: predictions: no validity", {
    model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    pred <- predictions(model)
    expect_predictions(pred, se = TRUE)
})

test_that("rlm: predictions: no validity", {
    skip_if_not_installed("insight", minimum_version = "0.14.4.1")
    model <- MASS::rlm(mpg ~ hp + drat, mtcars)
    pred <- predictions(model)
    expect_predictions(pred, se = TRUE, n_row = 1)
    pred <- predictions(model, newdata = head(mtcars))
    expect_predictions(pred, se = TRUE, n_row = 6)
})


### marginalmeans

test_that("glm.nb: marginalmeans: vs. emmeans", {
    dat <- mtcars
    dat$cyl <- as.factor(dat$cyl)
    dat$am <- as.logical(dat$am)
    model <- suppressWarnings(MASS::glm.nb(carb ~ am + cyl, data = dat))
    mm <- marginalmeans(model, type = "link", variables = "cyl")
    ti <- tidy(mm)
    em <- tidy(emmeans::emmeans(model, "cyl"))
    expect_marginalmeans(mm, se = TRUE)
    expect_equal(ti$estimate, em$estimate)
    expect_equal(ti$std.error, em$std.error)
})

library(testthat)
test_that("rlm: marginalmeans: vs. emmeans", {
    skip_if_not_installed("insight", minimum_version = "0.14.4.1")
    dat <- mtcars
    dat$cyl <- as.factor(dat$cyl)
    dat$am <- as.logical(dat$am)
    model <- MASS::rlm(mpg ~ cyl + am, dat)
    mm <- marginalmeans(model)
    expect_marginalmeans(mm, se = TRUE)
    ti <- tidy(marginalmeans(model, variables = "cyl"))
    em <- tidy(emmeans::emmeans(model, "cyl"))
    expect_equal(ti$estimate, em$estimate)
    expect_equal(ti$std.error, em$std.error)
})
