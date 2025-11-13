testthat::skip_if_not_installed("lmerTest")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")
testthat::skip_if_not_installed("margins")

requiet("lmerTest")
requiet("emmeans")
requiet("broom")
requiet("margins")

# Basic expectation tests
mod_simple <- lmerTest::lmer(mpg ~ wt + am + (1|cyl), data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# vs. emmeans vs. margins
dat_lmerTest <- read.csv(testing_path("stata/databases/lme4_02.csv"))
mod <- lme4::lmer(y ~ x1 * x2 + (1 | clus), data = dat_lmerTest)

# no validity
expect_slopes2(mod)
expect_predictions2(mod)

# emmeans
em <- suppressMessages(emmeans::emtrends(mod, ~x1, "x1", at = list(x1 = 0, x2 = 0)))
em <- tidy(em)
me <- avg_slopes(mod, newdata = datagrid(x1 = 0, x2 = 0, clus = 1))
expect_equal(me$std.error[1], em$std.error, tolerance = .01, ignore_attr = TRUE)
expect_equal(me$estimate[1], em$x1.trend, ignore_attr = TRUE)

# margins
me <- avg_slopes(mod)
ma <- margins(mod)
ma <- tidy(ma)
expect_equal(me$std.error, ma$std.error, tolerance = .0001, ignore_attr = TRUE)
expect_equal(me$estimate, ma$estimate, ignore_attr = TRUE)


# bug: population-level predictions() when {lmerTest} is loaded
requiet("lmerTest")
mod <- suppressMessages(lmer(
    weight ~ 1 + Time + I(Time^2) + Diet + Time:Diet + I(Time^2):Diet + (1 + Time + I(Time^2) | Chick),
    data = ChickWeight
))
expect_s3_class(predictions(mod, newdata = datagrid(Chick = NA, Diet = 1:4, Time = 0:21), re.form = NA), "predictions")

expect_s3_class(
    predictions(mod, newdata = datagrid(Diet = 1:4, Time = 0:21), re.form = NA),
    "predictions"
)
