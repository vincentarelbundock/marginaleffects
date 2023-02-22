source("helpers.R")
using("marginaleffects")

requiet("lmerTest")
requiet("emmeans")
requiet("broom")
requiet("margins")

# vs. emmeans vs. margins
dat <- read.csv(testing_path("stata/databases/lme4_02.csv"))
mod <-lme4::lmer(y ~ x1 * x2 + (1 | clus), data = dat)

# no validity
expect_slopes(mod)
expect_predictions(predictions(mod))

# emmeans
em <- suppressMessages(emmeans::emtrends(mod, ~x1, "x1", at = list(x1 = 0, x2 = 0)))
em <- tidy(em)
me <- slopes(mod, newdata = datagrid(x1 = 0, x2 = 0, clus = 1))
me <- tidy(me)
expect_equivalent(me$std.error[1], em$std.error, tolerance = .01)
expect_equivalent(me$estimate[1], em$x1.trend)

# margins
me <- slopes(mod)
me <- tidy(me)
ma <- margins(mod)
ma <- tidy(ma)
expect_equivalent(me$std.error, ma$std.error, tolerance = .0001)
expect_equivalent(me$estimate, ma$estimate)



# bug: population-level predictions() when {lmerTest} is loaded
requiet("lmerTest")
mod <- suppressMessages(lmer(
  weight ~ 1 + Time + I(Time^2) + Diet + Time:Diet + I(Time^2):Diet + (1 + Time + I(Time^2) | Chick),
  data = ChickWeight))
expect_inherits(predictions(mod,
                       newdata = datagrid(Chick = NA,
                                          Diet = 1:4,
                                          Time = 0:21),
                       re.form = NA),
           "predictions")

expect_inherits(
    predictions(mod,
        newdata = datagrid(Diet = 1:4,
                           Time = 0:21),
        re.form = NA),
    "predictions")



rm(list = ls())