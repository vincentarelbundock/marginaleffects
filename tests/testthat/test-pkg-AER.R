test_that("AER package works", {
    skip_if_not_installed("AER")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("AER")
    withr_library("emmeans")
    withr_library("broom")

    tol_se <- 1e-4

    dat <- get_dataset("Affairs", "AER")

    # tobit: marginaleffects vs. Stata
    stata <- readRDS(test_path("stata/stata.rds"))$aer_tobit
    mod1 <- tobit(
        affairs ~ age + yearsmarried + religiousness + occupation + rating,
        data = dat
    )
    mfx <- merge(tidy(slopes(mod1, newdata = dat)), stata)
    slo <- slopes(mod1, newdata = dat)
    expect_s3_class(slo, "slopes")
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .00001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = 1e-4, ignore_attr = TRUE)

    stata <- readRDS(test_path("stata/stata.rds"))$aer_tobit_right
    mod2 <- tobit(
        affairs ~ age + yearsmarried + religiousness + occupation + rating,
        right = 4,
        data = dat
    )
    mfx <- merge(tidy(slopes(mod2, newdata = dat)), stata)
    slo <- slopes(mod2, newdata = dat)
    expect_s3_class(slo, "slopes")
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .1, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .1, ignore_attr = TRUE)

    # marginaleffects vs. emtrends
    mod <- tobit(affairs ~ age + yearsmarried, data = dat)
    mfx <- slopes(mod, newdata = datagrid(age = 30, yearsmarried = 5))
    em1 <- emmeans::emtrends(mod, ~age, "age", at = list(age = 30, yearsmarried = 5))
    em2 <- emmeans::emtrends(mod, ~yearsmarried, "yearsmarried", at = list(age = 30, yearsmarried = 5))
    em1 <- tidy(em1)
    em2 <- tidy(em2)
    expect_equal(mfx$estimate[1], em1$age.trend, ignore_attr = TRUE)
    expect_equal(mfx$std.error[1], em1$std.error, tolerance = .001, ignore_attr = TRUE)
    expect_equal(mfx$estimate[2], em2$yearsmarried.trend, ignore_attr = TRUE)
    expect_equal(mfx$std.error[2], em2$std.error, tolerance = .0002, ignore_attr = TRUE)

    # predictions: tobit: no validity
    mod <- AER::tobit(
        affairs ~ age + yearsmarried + religiousness + occupation + rating,
        data = dat
    )
    pred <- predictions(mod, newdata = dat)
    expect_s3_class(pred, "predictions")
    expect_equal(nrow(pred), nrow(dat), ignore_attr = TRUE)
})
