testthat::skip_if_not_installed("flexsurv")
requiet("flexsurv")

# Basic expectation tests
mod_simple <- flexsurv::flexsurvreg(Surv(futime, fustat) ~ age, data = ovarian, dist = "weibull")
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

mod <- flexsurvreg(formula = Surv(futime, fustat) ~ age + ecog.ps, data = ovarian, dist = "gengamma")
x <- avg_slopes(mod)
expect_s3_class(x, "slopes")
x <- predictions(mod)
expect_s3_class(x, "predictions")
x <- comparisons(mod)
expect_s3_class(x, "comparisons")
