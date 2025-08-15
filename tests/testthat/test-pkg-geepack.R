test_that("geepack package works", {
    skip_if_not_installed("geepack")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("geepack")
    withr_library("emmeans")
    withr_library("broom")

    # Stata does not replicate coefficients exactly:
    # xtset Pig Time
    # xtgee Weight i.Cu, family(poisson) link(identity) corr(ar 1)

    # geepack::geeglm: marginaleffects vs. emtrends
    data(dietox, package = "geepack")
    dietox$Cu <- as.factor(dietox$Cu)
    mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
    model <- suppressWarnings(geeglm(mf, data = dietox, id = Pig, family = poisson("identity"), corstr = "ar1"))
    slo <- slopes(model)
    expect_s3_class(slo, "slopes")
    # emmeans
    mfx <- slopes(model, variables = "Time", newdata = datagrid(Time = 10, Cu = "Cu000"), type = "link")
    em <- suppressMessages(emtrends(model, ~Time, var = "Time", at = list(Time = 10, Cu = "Cu000")))
    em <- tidy(em)
    expect_equal(mfx$estimate, em$Time.trend, tolerance = .001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, em$std.error, tolerance = .01, ignore_attr = TRUE)

    # predictions: geepack::geeglm: no validity
    data(dietox, package = "geepack")
    dietox$Cu <- as.factor(dietox$Cu)
    mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
    model <- suppressWarnings(geeglm(mf, data = dietox, id = Pig, family = poisson("identity"), corstr = "ar1"))
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(dietox))
    expect_s3_class(pred1, "predictions")
    expect_equal(nrow(pred1), nrow(dietox), ignore_attr = TRUE)
    expect_s3_class(pred2, "predictions")
    expect_equal(nrow(pred2), 6, ignore_attr = TRUE)

    # TODO: why no support for standard errors?
    # marginalmeans: geepack::geeglm: vs. emmeans
    data(dietox, package = "geepack")
    dietox$Cu <- as.factor(dietox$Cu)
    mf <- formula(Weight ~ Cu + Time + I(Time^2) + I(Time^3))
    model <- suppressWarnings(geeglm(mf, data = dietox, id = Pig, family = poisson("identity"), corstr = "ar1"))

    em <- tidy(emmeans::emmeans(model, ~Cu, df = Inf, at = list(Time = 10)), type = "response")
    pr <- predictions(model, datagrid(Time = 10, Cu = unique))
    expect_equal(em$estimate, pr$estimate, ignore_attr = TRUE)
    expect_equal(em$std.error, pr$std.error, tolerance = 1e-5, ignore_attr = TRUE)

    # TODO: not clear where `emmeans` holds the Time variable
    # em <- emmeans::emmeans(model, ~Cu, type = "response", df = Inf)
    # em <- data.frame(em)
    # expect_equal(mm$estimate, em$emmean)
    # expect_equal(mm$conf.low, em$asymp.LCL)
    # expect_equal(mm$conf.high, em$asymp.UCL)
})
