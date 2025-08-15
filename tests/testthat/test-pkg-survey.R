test_that("survey package works", {
    skip_if_not_installed("margins")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")
    skip_if_not_installed("survey")

    withr_library("margins")
    withr_library("emmeans")
    withr_library("broom")
    withr_library("survey")

    # survey: marginaleffects vs. margins vs. emtrends
    data("fpc", package = "survey")
    svyd <- survey::svydesign(
        weights = ~weight,
        ids = ~psuid,
        strata = ~stratid,
        fpc = ~Nh,
        variables = ~ x + nh,
        data = fpc,
        nest = TRUE
    )
    mod <- survey::svyglm(x ~ nh, design = svyd)
    res <- slopes(mod, wts = "(weights)")
    mar <- suppressMessages(data.frame(margins(mod, unit_ses = TRUE)))
    expect_equal(res$estimate, as.numeric(mar$dydx_nh), ignore_attr = TRUE)
    expect_equal(res$std.error, as.numeric(mar$SE_dydx_nh), tolerance = 0.001, ignore_attr = TRUE)

    # emtrends
    em <- emtrends(mod, ~nh, "nh", at = list(nh = 4))
    em <- tidy(em)
    mfx <- slopes(mod, type = "link", newdata = data.frame(nh = 4))
    expect_equal(mfx$estimate, em$nh.trend, tolerance = .001, ignore_attr = TRUE) # CRAN tolerance
    expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)

    # Issue #1131
    data("lalonde", package = "MatchIt")
    fit <- survey::svyglm(re78 ~ treat, design = survey::svydesign(~1, weights = ~1, data = lalonde))
    p <- marginaleffects::get_predict(fit, newdata = lalonde)
    expect_s3_class(p, "data.frame")

    # Issue #1161
    dat <- get_dataset("SmokeBan", "AER")
    dat$weights <- runif(n = nrow(dat), min = 1, max = 100)
    dat$smoker <- factor(dat$smoker)
    design1 = svydesign(ids = ~1, weights = ~weights, data = dat)
    m <- suppressWarnings(svyglm(
        smoker ~ ban * education * gender + age,
        design = design1,
        family = binomial(),
        data = dat
    ))
    cmp <- avg_comparisons(
        m,
        variables = "education",
        by = c("ban", "gender"),
        wts = "weights",
        hypothesis = ~reference
    )
    expect_false(anyNA(cmp$estimate))
})
