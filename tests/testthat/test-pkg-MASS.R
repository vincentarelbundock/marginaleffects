skip_if_not_installed("MASS")
skip_if_not_installed("margins")

requiet("margins")
requiet("MASS")

test_that("MASS::rlm no validity check", {
    model <- MASS::rlm(mpg ~ hp + drat, mtcars)
    mfx <- marginaleffects(model)
    expect_mfx(model, n_unique = 1)
})




test_that("glm.nb vs. margins", {
    # margins does not support unit-level standard errors
    model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    mfx <- marginaleffects(model)
    mar <- margins(model)
    expect_true(test_against_margins(mfx, mar))
})


test_that("glm.nb vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$mass_glm_nb
    model <- suppressWarnings(
        MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
})


test_that("polr vs. Stata", {
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
