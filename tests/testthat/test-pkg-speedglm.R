test_that("speedglm package works", {
    skip_if_not_installed("speedglm")
    skip_if_not_installed("margins")

    withr_library("speedglm")
    withr_library("margins")

    # glm vs. Stata
    stata <- readRDS(test_path("stata/stata.rds"))[["stats_glm_01"]]
    dat <- read.csv(test_path("stata/databases/stats_glm_01.csv"))
    mod <- speedglm(y ~ x1 * x2, family = binomial(), data = dat)
    mfx <- merge(avg_slopes(mod), stata)
    slo <- slopes(mod)
    expect_s3_class(slo, "slopes")
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001, ignore_attr = TRUE)

    # margins: wrong standard errors
    mfx <- slopes(mod)
    mar <- margins(mod, unit_ses = TRUE)
    # Note: expect_margins is a custom function, using basic comparison
    expect_true(all(abs(mfx$estimate - mar$dydx) < 0.001))

    # lm vs. Stata
    stata <- readRDS(test_path("stata/stata.rds"))[["stats_lm_01"]]
    dat <- read.csv(test_path("stata/databases/stats_lm_01.csv"))
    mod <- speedlm(y ~ x1 * x2, data = dat)
    mfx <- merge(avg_slopes(mod), stata)
    slo <- slopes(mod)
    expect_s3_class(slo, "slopes")
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .00001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001, ignore_attr = TRUE)

    # margins: wrong standard errors
    mfx <- slopes(mod)
    mar <- margins(mod, unit_ses = TRUE)
    # Note: expect_margins is a custom function, using basic comparison
    expect_true(all(abs(mfx$estimate - mar$dydx) < 1e-3))
})
