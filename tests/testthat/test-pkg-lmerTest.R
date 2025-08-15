test_that("lmerTest package works", {
    skip_if_not_installed("lmerTest")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")
    skip_if_not_installed("margins")

    withr_library("lmerTest")
    withr_library("emmeans")
    withr_library("broom")
    withr_library("margins")

    # vs. emmeans vs. margins
    dat <- read.csv(test_path("stata/databases/lme4_02.csv"))
    mod <- lme4::lmer(y ~ x1 * x2 + (1 | clus), data = dat)

    # no validity
    slo <- slopes(mod)
    expect_s3_class(slo, "slopes")
    pre <- predictions(mod)
    expect_s3_class(pre, "predictions")

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
    mod <- suppressMessages(lmer(
        weight ~ 1 + Time + I(Time^2) + Diet + Time:Diet + I(Time^2):Diet + (1 + Time + I(Time^2) | Chick),
        data = ChickWeight
    ))
    pre1 <- predictions(mod, newdata = datagrid(Chick = NA, Diet = 1:4, Time = 0:21), re.form = NA)
    expect_s3_class(pre1, "predictions")

    pre2 <- predictions(mod, newdata = datagrid(Diet = 1:4, Time = 0:21), re.form = NA)
    expect_s3_class(pre2, "predictions")
})
