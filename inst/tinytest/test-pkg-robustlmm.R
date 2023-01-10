source("helpers.R")
using("marginaleffects")

exit_if_not(requiet("robustlmm"))
exit_if_not(requiet("emmeans"))
exit_if_not(requiet("broom"))

# no validity
mod <- rlmer(Reaction ~ Days + (Days | Subject), sleepstudy,
    rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
    rho.sigma.b = chgDefaults(smoothPsi, k = 5.11, s = 10))
expect_predictions(predictions(mod))
expect_slopes(mod, n_unique = 1)
