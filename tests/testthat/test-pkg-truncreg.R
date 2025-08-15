test_that("truncreg package works", {
    skip_if_not_installed("truncreg")
    skip_if_not_installed("margins")

    withr_library("truncreg")
    withr_library("margins")

    # truncreg: no validity check
    data("tobin", package = "survival")
    model <- truncreg(durable ~ age + quant, data = tobin, subset = durable > 0)
    tid <- avg_slopes(model)
    expect_s3_class(tid, "data.frame")
    expect_equal(nrow(tid), 2, ignore_attr = TRUE)
    expect_false(any(tid$estimate == 0))
    expect_false(anyNA(tid$estimate))
    expect_false(any(tid$std.error == 0))
    expect_false(anyNA(tid$std.error))

    # truncreg vs. Stata
    # numeric differences could be resolved with different tolerance, but
    # finding the correct threshold by trial and error is difficult on CRAN
    stata <- readRDS(test_path("stata/stata.rds"))$truncreg_truncreg_01
    data("tobin", package = "survival")
    model <- truncreg::truncreg(durable ~ age + quant, data = tobin, subset = durable > 0)
    mfx <- merge(avg_slopes(model), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001, ignore_attr = TRUE)

    # margins
    mar <- margins(model, unit_ses = TRUE)
    mfx <- slopes(model)
    # Note: expect_margins is a custom function, using basic comparison
    expect_true(all(abs(mfx$estimate - mar$dydx) < 0.001))
})
