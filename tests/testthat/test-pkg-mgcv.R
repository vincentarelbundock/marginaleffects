requiet("mgcv")
requiet("emmeans")
requiet("broom")

test_that("marginaleffects vs. emtrends", {
    set.seed(2)
    void <- capture.output(dat <- gamSim(1, n = 400, dist = "normal", scale = 2))
    void <- capture.output(dat2 <- gamSim(1, n = 2000, dist = "poisson", scale = .1))
    m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
    m2 <- gam(y ~ te(x0, x1, k = 7) + s(x2) + s(x3), data = dat, method = "REML")
    m3 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3) + ti(x1, x2, k = 6), data = dat, method = "REML")
    m4 <- gam(y ~ s(x0, x1, k = 40) + s(x2) + s(x3), data = dat, method = "REML")
    m5 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML", select = TRUE)
    m6 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), sp = c(0.01, -1, -1, -1), data = dat)
    m7 <- gam(y ~ s(x0, sp = .01) + s(x1) + s(x2) + s(x3), data = dat)
    m8 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), min.sp = c(0.001, 0.01, 0, 10), data = dat)
    m9 <- gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + s(x2, bs = "cr") +
              s(x3, bs = "cr"), family = poisson, data = dat2, method = "REML")
    expect_marginaleffects(m1)
    expect_marginaleffects(m2)
    expect_marginaleffects(m3)
    expect_marginaleffects(m4)
    expect_marginaleffects(m5)
    expect_marginaleffects(m6)
    expect_marginaleffects(m7)
    expect_marginaleffects(m8)
    expect_marginaleffects(m9)

    # emtrends
    mfx <- marginaleffects(m1, variables = "x1", newdata = datagrid(x1 = 0, x2 = 0, x3 = 0), type = "link")
    em <- emtrends(m1, ~x1, "x1", at = list(x1 = 0, x2 = 0, x3 = 0))
    em <- tidy(em)
    expect_equal(mfx$dydx, em$x1.trend)
    expect_equal(mfx$std.error, em$std.error, tolerance = .0001)
})


test_that("predictions: no validity", {
    set.seed(2)
    void <- capture.output(dat <- gamSim(1, n = 400, dist = "normal", scale = 2))
    mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
    pred1 <- predictions(mod)
    pred2 <- predictions(mod, newdata = head(dat))
    expect_predictions(pred1, n_row = nrow(dat))
    expect_predictions(pred2, n_row = 6)
})


test_that("exclude a smooth", {
    requiet("itsadug")
    data(simdat)
    simdat$Subject <- as.factor(simdat$Subject)
    model <- bam(Y ~ Group + s(Time, by = Group) + s(Subject, bs = "re"), data = simdat)
    nd <- datagrid(model = model,
                   Subject = "a01",
                   Group = "Adults")

    expect_equal(predictions(model, newdata = nd)$predicted,
                 predict(model, newdata = nd),
                 ignore_attr = TRUE)

    expect_equal(predictions(model, newdata = nd, exclude = "s(Subject)")$predicted,
                 predict(model, newdata = nd, exclude = "s(Subject)"),
                 ignore_attr = TRUE)
})
