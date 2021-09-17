skip_if_not_installed("bife")

test_that("bife: no validity check", {
    requiet("bife")
    dataset <- psid
    mod <- bife(LFP ~ AGE + I(INCH / 1000) + KID1 + KID2 + KID3 | ID, data = dataset)
    mfx <- marginaleffects(mod)
    tid <- tidy(mfx)
    expect_s3_class(tid, "data.frame")
    expect_true("std.error" %in% colnames(tid))
    expect_false(any(tid$estimate == 0))
    expect_false(any(tid$std.error == 0))
})
