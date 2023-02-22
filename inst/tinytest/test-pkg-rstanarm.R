# HPD tests against emmeans, which uses HDI, but our default is ETI
# HDI is implemented specifically for these tests
# https://github.com/vincentarelbundock/marginaleffects/issues/240
source("helpers.R")
using("marginaleffects")

options("marginaleffects_posterior_interval" = "hdi")
requiet("rstanarm")
requiet("emmeans")
requiet("margins")
requiet("broom")
if (!require("rstanarm")) exit_file("rstanarm") # after requiet to avoid messages

# interactions
void <- capture.output(
  mod <- stan_glm(am ~ hp + mpg * vs, data = mtcars, family = binomial(link = "logit"))
)
expect_slopes(mod, se = FALSE)
expect_predictions(predictions(mod), se = FALSE)

# no interactions
void <- capture.output(
  mod <- stan_glm(am ~ hp + mpg + vs, data = mtcars, family = binomial(link = "logit"))
)

# emtrends
mfx <- slopes(mod, newdata = datagrid(hp = 110, mpg = 20, vs = 0), variables = "hp", type = "link")
em <- emtrends(mod, ~hp, "hp", at = list(hp = 110, mpg = 20, vs = 0))
em <- tidy(em)
expect_equivalent(mfx$estimate, em$hp.trend)
expect_equivalent(mfx$conf.low, em$lower.HPD, tolerance = 1e-5)
expect_equivalent(mfx$conf.high, em$upper.HPD)

options("marginaleffects_posterior_interval" = "eti")

# # margins: var is all zeroes and dydx don't match precisely
# mar <- margins(mod, unit_ses = TRUE, at = list(hp = 110, mpg = 20, vs = 0))
# mfx <- slopes(mod, variables = "hp", at = list(hp = 110, mpg = 20, vs = 0))
# expect_equivalent(mfx$estimate, mar$dydx_hp)
# expect_equivalent(mfx$std.error, mar$dydx_hp)



rm(list = ls())