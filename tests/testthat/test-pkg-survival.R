requiet("survival")
requiet("emmeans")
requiet("broom")

test_that("coxph vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$survival_coxph_01
    test1 <<- data.frame(time = c(4, 3, 1, 1, 2, 2, 3),
                         status = c(1, 1, 1, 0, 1, 1, 0),
                         x = c(0, 2, 1, 1, 1, 0, 0),
                         sex = factor(c(0, 0, 0, 0, 1, 1, 1)))
    mod <- coxph(Surv(time, status) ~ x + strata(sex),
                 data = test1,
                 ties = "breslow")
    mfx <- merge(tidy(marginaleffects(mod, type = "lp")), stata)
    expect_marginaleffects(mod, type = "risk", n_unique = 4)
    expect_equal(mfx$estimate, mfx$dydxstata)
    expect_equal(mfx$std.error, mfx$std.errorstata)

    # emtrends
    em <- emtrends(mod, ~x, "x", at = list(time = 4, status = 1, x = 0, sex = factor(0, levels = 0:1)))
    em <- tidy(em)
    mfx <- marginaleffects(mod, variables = "x", type = "lp")
    expect_equal(mfx$dydx[1], em$x.trend)
    expect_equal(mfx$std.error[1], em$std.error)
})


test_that("coxph: no validity", {
    test2 <<- data.frame(start = c(1, 2, 5, 2, 1, 7, 3, 4, 8, 8),
                         stop = c(2, 3, 6, 7, 8, 9, 9, 9, 14, 17),
                         event = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
                         x = c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0))
    mod <- coxph(Surv(start, stop, event) ~ x, test2)
    expect_marginaleffects(mod, type = "risk", n_unique = 2)
})


test_that("bugs stay dead: conf.level forces get_predicted which doesn't process 'type'", {
    test1 <<- data.frame(time = c(4, 3, 1, 1, 2, 2, 3),
                         status = c(1, 1, 1, 0, 1, 1, 0),
                         x = c(0, 2, 1, 1, 1, 0, 0),
                         sex = factor(c(0, 0, 0, 0, 1, 1, 1)))
    mod <- coxph(Surv(time, status) ~ x + strata(sex),
                 data = test1,
                 ties = "breslow")
    p1 <- predictions(mod, type = "lp")
    p2 <- predictions(mod, type = "risk")
    expect_true(all(p1$predicted != p2$predicted))
})


test_that("bugs stay dead: numeric vs factor strata", {
    skip_if_not_installed("insight", minimum_version = "0.17.0")
    stata <- readRDS(test_path("stata/stata.rds"))$survival_coxph_01
    test1 <<- data.frame(time = c(4, 3, 1, 1, 2, 2, 3),
                         status = c(1, 1, 1, 0, 1, 1, 0),
                         x = c(0, 2, 1, 1, 1, 0, 0),
                         sex = factor(c(0, 0, 0, 0, 1, 1, 1)))
    test2 <<- data.frame(time = c(4, 3, 1, 1, 2, 2, 3),
                         status = c(1, 1, 1, 0, 1, 1, 0),
                         x = c(0, 2, 1, 1, 1, 0, 0),
                         sex = c(0, 0, 0, 0, 1, 1, 1))
    mod1 <- coxph(Surv(time, status) ~ x + strata(sex),
                  data = test1,
                  ties = "breslow")
    mod2 <- coxph(Surv(time, status) ~ x + strata(sex),
                  data = test2,
                  ties = "breslow")
    mfx1 <- merge(tidy(marginaleffects(mod1, type = "lp")), stata)
    mfx2 <- merge(tidy(marginaleffects(mod2, type = "lp")), stata)
    expect_equal(mfx1$dydx, mfx2$dydx)
})

