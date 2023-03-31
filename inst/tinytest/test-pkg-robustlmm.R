exit_file("Matrix version breakage")

source("helpers.R")
using("marginaleffects")

requiet("robustlmm")
requiet("emmeans")
requiet("broom")

# no validity
mod <- rlmer(Reaction ~ Days + (Days | Subject), sleepstudy,
    rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
    rho.sigma.b = chgDefaults(smoothPsi, k = 5.11, s = 10))
expect_predictions(predictions(mod))
expect_slopes(mod, n_unique = 1)


source("helpers.R")
rm(list = ls())