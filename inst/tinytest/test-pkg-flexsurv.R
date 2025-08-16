source("helpers.R")
using("marginaleffects")
requiet("flexsurv")

# Basic expectation tests
mod_simple <- flexsurv::flexsurvreg(Surv(futime, fustat) ~ age, data = ovarian, dist = "weibull")
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

mod <- flexsurvreg(formula = Surv(futime, fustat) ~ age + ecog.ps, data = ovarian, dist = "gengamma")
x <- avg_slopes(mod)
expect_inherits(x, "slopes")
x <- predictions(mod)
expect_inherits(x, "predictions")
x <- comparisons(mod)
expect_inherits(x, "comparisons")
