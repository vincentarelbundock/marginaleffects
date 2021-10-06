skip_if_not_installed("robustbase")
skip_if_not_installed("margins")
requiet("robustbase")
requiet("margins")


test_that("lmrob vs. margins", {
    data(coleman, package = "robustbase")
    model <- lmrob(Y ~ ., data=coleman, setting = "KS2014")
    expect_marginaleffects(model, n_unique = 1)
    mar <- margins::margins(model, unit_ses = TRUE)
    mfx <- marginaleffects(model)
    expect_true(test_against_margins(mar, mfx))
})


test_that("glmrob vs. margins", {
    data(epilepsy, package = "robustbase")
    model <- glmrob(Ysum ~ Age10 + Base4*Trt, family = poisson,
                    data = epilepsy, method= "Mqle",
                    control = glmrobMqle.control(tcc= 1.2))
    expect_marginaleffects(model)
    mar <- margins::margins(model, unit_ses = TRUE)
    mfx <- marginaleffects(model)
    expect_true(test_against_margins(mar, mfx))
})
