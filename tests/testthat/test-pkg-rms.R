skip_if_not_installed("rms")

test_that("rms: no validity check", {
    model <- rms::lrm(am ~ mpg, mtcars)
    # the usual prediction types don't work in predict.rlm
    void <- capture.output(
        mfx <- marginaleffects(model, type = "lp")
    )
    expect_s3_class(mfx, "data.frame")
})
