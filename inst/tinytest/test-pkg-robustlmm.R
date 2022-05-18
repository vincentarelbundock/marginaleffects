source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("robustlmm")
requiet("emmeans")
requiet("broom")

# no validity
mod <- rlmer(Reaction ~ Days + (Days | Subject), sleepstudy,
    rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
    rho.sigma.b = chgDefaults(smoothPsi, k = 5.11, s = 10))
expect_predictions(predictions(mod))
expect_marginaleffects(mod, n_unique = 1)
