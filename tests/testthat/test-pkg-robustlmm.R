requiet("robustlmm")
requiet("emmeans")
requiet("broom")

test_that("no validity", {
    mod <- rlmer(Reaction ~ Days + (Days | Subject), sleepstudy,
        rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
        rho.sigma.b = chgDefaults(smoothPsi, k = 5.11, s = 10))
    expect_predictions(predictions(mod))
    expect_marginaleffects(mod, n_unique = 1)
})
