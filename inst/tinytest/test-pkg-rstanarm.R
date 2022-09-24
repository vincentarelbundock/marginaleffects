# HPD tests against emmeans, which uses HDI, but our default is ETI
# HDI is implemented specifically for these tests
# https://github.com/vincentarelbundock/marginaleffects/issues/240
source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
options("marginaleffects_credible_interval" = "hdi")
requiet("rstanarm")
requiet("emmeans")
requiet("margins")
requiet("broom")
if (!require("rstanarm")) exit_file("rstanarm") # after requiet to avoid messages

# interactions
void <- capture.output(
  mod <- stan_glm(am ~ hp + mpg * vs, data = mtcars, family = binomial(link = "logit"))
)
expect_marginaleffects(mod, se = FALSE)
expect_predictions(predictions(mod), se = FALSE)

# no interactions
void <- capture.output(
  mod <- stan_glm(am ~ hp + mpg + vs, data = mtcars, family = binomial(link = "logit"))
)

# emtrends
mfx <- marginaleffects(mod, newdata = datagrid(hp = 110, mpg = 20, vs = 0), variables = "hp", type = "link")
em <- emtrends(mod, ~hp, "hp", at = list(hp = 110, mpg = 20, vs = 0))
em <- tidy(em)
expect_equivalent(mfx$dydx, em$hp.trend)
expect_equivalent(mfx$conf.low, em$lower.HPD, tolerance = 1e-5)
expect_equivalent(mfx$conf.high, em$upper.HPD)

# # margins: var is all zeroes and dydx don't match precisely
# mar <- margins(mod, unit_ses = TRUE, at = list(hp = 110, mpg = 20, vs = 0))
# mfx <- marginaleffects(mod, variables = "hp", at = list(hp = 110, mpg = 20, vs = 0))
# expect_equivalent(mfx$dydx, mar$dydx_hp)
# expect_equivalent(mfx$std.error, mar$dydx_hp)

