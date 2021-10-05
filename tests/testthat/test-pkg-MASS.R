skip_if_not_installed("MASS")
skip_if_not_installed("margins")

requiet("margins")
requiet("MASS")


# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
# |m|a|r|g|i|n|a|l|e|f|f|e|c|t|s|
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


test_that("rlm: marginaleffects: vs. margins", {
    model <- MASS::rlm(mpg ~ hp + drat, mtcars)
    mfx <- marginaleffects(model)
    expect_mfx(model, n_unique = 1)
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
    skip("works in interactive session")
    stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    mod <- MASS::polr(factor(y) ~ x1 + x2, data = dat)
    mfx <- marginaleffects(mod, type = "probs")
    mfx <- tidy(mfx)
    mfx <- merge(mfx, stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
    expect_mfx(mod, type = "probs")
})


#                     _ _      _   _                 
#  _ __  _ __ ___  __| (_) ___| |_(_) ___  _ __  ___ 
# | '_ \| '__/ _ \/ _` | |/ __| __| |/ _ \| '_ \/ __|
# | |_) | | |  __/ (_| | | (__| |_| | (_) | | | \__ \
# | .__/|_|  \___|\__,_|_|\___|\__|_|\___/|_| |_|___/
# |_|                                                



test_that("polr: predictions: no validity", {
    skip("polr: predictions doesn't work. Probably needs to be fixed at the `insight` level.")
    mod <- MASS::polr(factor(gear) ~ mpg + factor(cyl), data = mtcars)
    pred <- predictions(mod)
})

test_that("glm.nb: predictions: no validity", {
    model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    pred <- predictions(model)
    expect_predictions(pred, se = TRUE)
})

test_that("rlm: predictions: no validity", {
    skip("does MASS::rlm work with `get_predicted`?")
    model <- MASS::rlm(mpg ~ hp + drat, mtcars)
    pred <- predictions(model)
    expect_predictions(pred, se = TRUE)
})
