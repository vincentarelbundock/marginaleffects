requiet("lmerTest")
requiet("emmeans")
requiet("broom")
requiet("margins")

test_that("vs. emmeans vs. margins", {
    dat <- haven::read_dta(test_path("stata/databases/lme4_02.dta"))
    mod <- lmer(y ~ x1 * x2 + (1 | clus), data = dat)

    # no validity
    expect_marginaleffects(mod)
    expect_predictions(predictions(mod))

    # emmeans
    em <- emmeans::emtrends(mod, ~x1, "x1", at = list(x1 = 0, x2 = 0))
    em <- tidy(em)
    me <- marginaleffects(mod, newdata = datagrid(x1 = 0, x2 = 0, clus = 1))
    me <- tidy(me)
    expect_equal(me$std.error[1], em$std.error, tolerance = .01)
    expect_equal(me$estimate[1], em$x1.trend)

    # margins
    me <- marginaleffects(mod)
    me <- tidy(me)
    ma <- margins(mod)
    ma <- tidy(ma)
    expect_equal(me$std.error, ma$std.error, ignore_attr = TRUE)
    expect_equal(me$estimate, ma$estimate, ignore_attr = TRUE)
})
