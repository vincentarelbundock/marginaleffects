requiet("truncreg")


test_that("truncreg: no validity check", {
    data("tobin", package = "survival")
    model <- truncreg(durable ~ age + quant, data = tobin, subset = durable > 0)
    mfx <- marginaleffects(model)
    tid <- tidy(mfx)
    expect_s3_class(tid, "data.frame")
    expect_equal(nrow(tid), 2)
    expect_false(any(tid$estimate == 0))
    expect_false(anyNA(tid$estimate))
    expect_false(any(tid$std.error == 0))
    expect_false(anyNA(tid$std.error))
})


test_that("truncreg vs. Stata", {
    # numeric differences could be resolved with different tolerance, but
    # finding the correct threshold by trial and error is difficult on CRAN
    skip_on_cran()
    stata <- readRDS(test_path("stata/stata.rds"))$truncreg_truncreg_01
    data("tobin", package = "survival")
    model <- truncreg::truncreg(durable ~ age + quant, 
                                data = tobin, subset = durable > 0)
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
    # margins
    mar <- margins(model, unit_ses = TRUE)
    mfx <- marginaleffects(model)
    expect_true(test_against_margins(mfx, mar, tolerance = .0001))
})
