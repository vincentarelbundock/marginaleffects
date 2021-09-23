skip_if_not_installed("truncreg")
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
    stata <- readRDS(test_path("stata/stata.rds"))$truncreg_truncreg_01
    data("tobin", package = "survival")
    model <- truncreg::truncreg(durable ~ age + quant, 
                                data = tobin, subset = durable > 0)
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)
})
