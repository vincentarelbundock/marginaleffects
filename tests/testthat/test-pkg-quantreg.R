requiet("quantreg")

test_that("rg() no validity check", {
    mod <- quantreg::rq(mpg ~ hp * wt + factor(cyl), data = mtcars)
    expect_mfx(mod)
})


test_that("rq vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$quantreg_rq_01
    model <- quantreg::rq(mpg ~ hp * wt + factor(cyl), data = mtcars)
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)
})
