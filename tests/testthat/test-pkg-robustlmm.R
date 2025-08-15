test_that("robustlmm package works", {
    skip_if_not_installed("robustlmm")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")
    skip_if_not_installed("lme4")

    withr_library("robustlmm")
    withr_library("emmeans")
    withr_library("broom")
    withr_library("lme4")

    # no validity
    mod <- robustlmm::rlmer(
        Reaction ~ Days + (Days | Subject),
        sleepstudy,
        rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
        rho.sigma.b = chgDefaults(smoothPsi, k = 5.11, s = 10)
    )

    pred <- predictions(mod)
    expect_s3_class(pred, "predictions")

    slo <- slopes(mod)
    expect_s3_class(slo, "slopes")
})
