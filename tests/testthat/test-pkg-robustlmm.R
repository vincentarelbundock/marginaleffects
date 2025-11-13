testthat::skip_if_not_installed("robustlmm")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")
testthat::skip_if_not_installed("lme4")
requiet("robustlmm")
requiet("emmeans")
requiet("broom")
requiet("lme4")

# Basic expectation tests
data("sleepstudy", package = "lme4")
mod_simple <- robustlmm::rlmer(Reaction ~ Days + (1|Subject), data = sleepstudy)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# no validity
mod <- robustlmm::rlmer(
    Reaction ~ Days + (Days | Subject),
    sleepstudy,
    rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
    rho.sigma.b = chgDefaults(smoothPsi, k = 5.11, s = 10)
)
expect_predictions2(mod)
expect_slopes2(mod, n_unique = 1)
