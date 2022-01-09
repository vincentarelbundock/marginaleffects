requiet("speedglm")
requiet("margins")

test_that("glm vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))[["stats_glm_01"]]
    dat <- read.csv(test_path("stata/databases/stats_glm_01.csv"))
    mod <- speedglm(y ~ x1 * x2, family = binomial(), data = dat)
    mfx <- merge(tidy(marginaleffects(mod)), stata)
    expect_marginaleffects(mod)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .00001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)

    # margins: wrong standard errors
    mfx <- marginaleffects(mod)
    mar <- margins(mod, unit_ses = TRUE)
    expect_true(test_against_margins(mfx, mar, tolerance = .001))
})


test_that("lm vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))[["stats_lm_01"]]
    dat <- read.csv(test_path("stata/databases/stats_lm_01.csv"))
    mod <- speedlm(y ~ x1 * x2, data = dat)
    mfx <- merge(tidy(marginaleffects(mod)), stata)
    expect_marginaleffects(mod)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .00001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)

    # margins: wrong standard errors
    mfx <- marginaleffects(mod)
    mar <- margins(mod, unit_ses = TRUE)
    expect_true(test_against_margins(mfx, mar, tolerance = .0001))
})
