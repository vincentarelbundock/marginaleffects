# HPD tests against emmeans, which uses HDI, but our default is ETI
# HDI is implemented specifically for these tests
# https://github.com/vincentarelbundock/marginaleffects/issues/240
testthat::skip_if_not_installed("rstanarm")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("margins")
testthat::skip_if_not_installed("broom")

options("marginaleffects_posterior_interval" = "hdi")
requiet("rstanarm")
requiet("emmeans")
requiet("margins")
requiet("broom")

# interactions
void <- capture.output(
    mod <- stan_glm(am ~ hp + mpg * vs, data = mtcars, family = binomial(link = "logit"))
)
expect_slopes2(mod, se = FALSE)
expect_predictions2(mod, se = FALSE)
expect_comparisons2(mod, se = FALSE)
# expect_hypotheses2(mod, se = FALSE)

# no interactions
void <- capture.output(
    mod <- stan_glm(am ~ hp + mpg + vs, data = mtcars, family = binomial(link = "logit"))
)

# emtrends
mfx <- slopes(mod, newdata = datagrid(hp = 110, mpg = 20, vs = 0), variables = "hp", type = "link")
em <- emtrends(mod, ~hp, "hp", at = list(hp = 110, mpg = 20, vs = 0))
em <- tidy(em)
expect_equal(mfx$estimate, em$hp.trend, ignore_attr = TRUE)
expect_equal(mfx$conf.low, em$lower.HPD, tolerance = 1e-5, ignore_attr = TRUE)
expect_equal(mfx$conf.high, em$upper.HPD, ignore_attr = TRUE)

options("marginaleffects_posterior_interval" = "eti")

# # margins: var is all zeroes and dydx don't match precisely
# mar <- margins(mod, unit_ses = TRUE, at = list(hp = 110, mpg = 20, vs = 0))
# mfx <- slopes(mod, variables = "hp", at = list(hp = 110, mpg = 20, vs = 0))
# expect_equal(mfx$estimate, mar$dydx_hp, ignore_attr = TRUE)
# expect_equal(mfx$std.error, mar$dydx_hp, ignore_attr = TRUE)
