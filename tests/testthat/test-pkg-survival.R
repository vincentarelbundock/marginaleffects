skip_on_cran()
skip_if_not_installed("survival")
withr_library("survival")

test_that("# Issue #911: survreg support", {
    fit <- survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian, dist = "weibull", scale = 1)
    s <- avg_slopes(fit)
    expect_s3_class(s, "slopes")
})

test_that("clogit functionality", {
    # clogit
    N <- 10000
    ng <- 5000
    exd <- data.frame(
        g = rep(1:ng, each = N / ng),
        out = rep(0L:1L, N / 2),
        x = sample(0L:1L, N / 2, prob = c(.8, .2), replace = TRUE)
    )
    mod <- clogit(
        out ~ x + strata(g),
        method = "exact",
        data = exd
    )

    mfx <- slopes(mod, type = "lp")
    expect_s3_class(mfx, "marginaleffects")
    cmp <- comparisons(mod, type = "lp")
    expect_s3_class(cmp, "comparisons")
    pre <- predictions(mod, type = "lp")
    expect_s3_class(pre, "predictions")
})

test_that("survival package: coxph vs Stata comparison", {
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")
    withr_library("emmeans")
    withr_library("broom")

    # coxph vs. Stata
    stata <- readRDS(test_path("stata/stata.rds"))$survival_coxph_01
    test1 <- data.frame(
        time = c(4, 3, 1, 1, 2, 2, 3),
        status = c(1, 1, 1, 0, 1, 1, 0),
        x = c(0, 2, 1, 1, 1, 0, 0),
        sex = factor(c(0, 0, 0, 0, 1, 1, 1))
    )
    mod <- coxph(Surv(time, status) ~ x + strata(sex), data = test1, ties = "breslow")
    mfx <- merge(avg_slopes(mod, type = "lp"), stata)
    slo <- slopes(mod, type = "risk")
    expect_s3_class(slo, "slopes")
    expect_equal(mfx$estimate, mfx$dydxstata, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = 1e-5, ignore_attr = TRUE)

    # emtrends
    em <- emtrends(mod, ~x, "x", at = list(time = 4, status = 1, x = 0, sex = factor(0, levels = 0:1)))
    em <- tidy(em)
    mfx <- slopes(mod, variables = "x", type = "lp")
    expect_equal(mfx$estimate[1], em$x.trend, ignore_attr = TRUE)
    expect_equal(mfx$std.error[1], em$std.error, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that("coxph no validation", {
    test2 <- data.frame(
        start = c(1, 2, 5, 2, 1, 7, 3, 4, 8, 8),
        stop = c(2, 3, 6, 7, 8, 9, 9, 9, 14, 17),
        event = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
        x = c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0)
    )
    mod <- coxph(Surv(start, stop, event) ~ x, test2)
    slo <- slopes(mod, type = "risk")
    expect_s3_class(slo, "slopes")
})

test_that("bugs stay dead: conf.level forces get_predicted which doesn't process 'type'", {
    test3 <- data.frame(
        time = c(4, 3, 1, 1, 2, 2, 3),
        status = c(1, 1, 1, 0, 1, 1, 0),
        x = c(0, 2, 1, 1, 1, 0, 0),
        sex = factor(c(0, 0, 0, 0, 1, 1, 1))
    )
    mod <- coxph(Surv(time, status) ~ x + strata(sex), data = test3, ties = "breslow")
    p1 <- predictions(mod, type = "lp")
    p2 <- predictions(mod, type = "risk")
    expect_true(all(p1$estimate != p2$estimate))
})

test_that("bugs stay dead: numeric vs factor strata", {
    stata <- readRDS(test_path("stata/stata.rds"))$survival_coxph_01
    test4 <- data.frame(
        time = c(4, 3, 1, 1, 2, 2, 3),
        status = c(1, 1, 1, 0, 1, 1, 0),
        x = c(0, 2, 1, 1, 1, 0, 0),
        sex = factor(c(0, 0, 0, 0, 1, 1, 1))
    )
    test5 <- data.frame(
        time = c(4, 3, 1, 1, 2, 2, 3),
        status = c(1, 1, 1, 0, 1, 1, 0),
        x = c(0, 2, 1, 1, 1, 0, 0),
        sex = c(0, 0, 0, 0, 1, 1, 1)
    )
    mod1 <- coxph(Surv(time, status) ~ x + strata(sex), data = test4, ties = "breslow")
    mod2 <- coxph(Surv(time, status) ~ x + strata(sex), data = test5, ties = "breslow")

    mfx1 <- merge(avg_slopes(mod1, type = "lp"), stata)
    mfx2 <- merge(avg_slopes(mod2, type = "lp"), stata)
    expect_equal(mfx1$estimate, mfx2$estimate, ignore_attr = TRUE)
})

test_that("# Issue #1079: interaction effects", {
    set.seed(12345)
    aml <- survival::aml |>
        transform(z = rnorm(nrow(aml), 0, 1)) |>
        transform(zcat = cut(z, breaks = c(-10, -0.5, 0.5, 10)))
    mod <- coxph(Surv(time, status == 1) ~ x * zcat, data = aml)
    nd_n <- datagrid(model = mod, x = "Nonmaintained", zcat = unique)
    nd_m <- datagrid(model = mod, x = "Maintained", zcat = unique)
    p_n <- predict(mod, newdata = nd_n, type = "lp")
    p_m <- predict(mod, newdata = nd_m, type = "lp")
    e0 <- transform(nd_n, estimate = p_n - p_m)
    e1 <- comparisons(mod, variables = "x", newdata = datagrid(zcat = unique), type = "lp")
    e2 <- plot_comparisons(mod, variables = "x", condition = "zcat", type = "lp", draw = FALSE)
    e3 <- hypotheses(
        mod,
        hypothesis = c(
            "xNonmaintained + `xNonmaintained:zcat(0.5,10]` = 0",
            "xNonmaintained + `xNonmaintained:zcat(-0.5,0.5]` = 0",
            "xNonmaintained = 0"
        )
    )
    expect_equal(e0$estimate, e1$estimate, ignore_attr = TRUE)
    expect_equal(e0$estimate, e2$estimate, ignore_attr = TRUE)
    expect_equal(e0$estimate, e3$estimate, ignore_attr = TRUE)
})

test_that("Issue #1272: quantile predictions", {
    fit <- survreg(Surv(time, status) ~ ph.ecog + age + sex, lung, dist = "weibull")
    p1 <- avg_predictions(fit, variables = "sex", type = "quantile", p = 0.5)
    p2 <- avg_predictions(fit, variables = "sex", type = "quantile", p = 0.1)
    expect_true(all(p1$estimate > p2$estimate))
})

test_that("# Issue #1467: warning about anti-conservative standard errors", {
    skip_if_not_installed("splines")
    withr_library("splines")
    withr::local_options(marginaleffects_safe = TRUE)
    dat <- transform(survival::rotterdam, grande = factor(grade))
    mod <- coxph(
        Surv(dtime, death) ~ hormon * grade + ns(age, df = 2),
        data = dat
    )
    expect_warning(avg_comparisons(mod), pattern = "bootstrap")
    expect_warning(predictions(mod), pattern = "bootstrap")
})
