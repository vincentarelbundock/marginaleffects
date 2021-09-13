suppressPackageStartupMessages(library(truncreg, warn.conflicts = FALSE))

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
