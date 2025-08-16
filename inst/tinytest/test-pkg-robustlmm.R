source("helpers.R")
using("marginaleffects")

requiet("robustlmm")
requiet("emmeans")
requiet("broom")
requiet("lme4")

# Basic expectation tests
data("sleepstudy", package = "lme4")
mod_simple <- robustlmm::rlmer(Reaction ~ Days + (1|Subject), data = sleepstudy)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

# no validity
mod <- robustlmm::rlmer(
    Reaction ~ Days + (Days | Subject),
    sleepstudy,
    rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
    rho.sigma.b = chgDefaults(smoothPsi, k = 5.11, s = 10)
)
expect_predictions(mod)
expect_slopes(mod, n_unique = 1)
