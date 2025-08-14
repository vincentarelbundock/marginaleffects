skip_if_not_installed("robustbase")
skip_if_not_installed("margins")
withr_library("robustbase")

test_that("lmrob vs. margins comparison", {
    data(coleman, package = "robustbase")
    model <- lmrob(Y ~ ., data = coleman, setting = "KS2014")
    expect_s3_class(slopes(model), "slopes")
    mar <- margins::margins(model, unit_ses = TRUE)
    mfx <- slopes(model)
    expect_true(expect_margins(mar, mfx))
})


test_that("glmrob vs. margins comparison", {
    data(epilepsy, package = "robustbase")
    model <- glmrob(
        Ysum ~ Age10 + Base4 * Trt,
        family = poisson,
        data = epilepsy,
        method = "Mqle",
        control = glmrobMqle.control(tcc = 1.2)
    )
    expect_s3_class(slopes(model), "slopes")
    mar <- margins::margins(model, unit_ses = TRUE)
    mfx <- slopes(model)
    expect_true(expect_margins(mar, mfx))
})

