skip_if_not_installed("robustbase")
requiet("robustbase")


test_that("lmrob: no validity check", {
    data(coleman, package = "robustbase")
    model <- lmrob(Y ~ ., data=coleman, setting = "KS2014")
    mfx <- marginaleffects(model)
    tid <- marginaleffects(model)
    expect_s3_class(tid, "data.frame")
    expect_false(any(mfx$estimate == 0))
    expect_false(any(mfx$std.error == 0))
    expect_false(anyNA(mfx$std.error))
    expect_false(anyNA(mfx$estimate))
})


test_that("glmrob: no validity check", {
    data(epilepsy, package = "robustbase")
    model <- glmrob(Ysum ~ Age10 + Base4*Trt, family = poisson,
                    data = epilepsy, method= "Mqle",
                    control = glmrobMqle.control(tcc= 1.2))
    mfx <- marginaleffects(model)
    tid <- marginaleffects(model)
    expect_s3_class(tid, "data.frame")
    expect_false(any(mfx$estimate == 0))
    expect_false(any(mfx$std.error == 0))
    expect_false(anyNA(mfx$std.error))
    expect_false(anyNA(mfx$estimate))
})
