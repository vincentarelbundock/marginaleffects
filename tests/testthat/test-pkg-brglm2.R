test_that("brglm2 package works", {
    skip_if_not_installed("brglm2")
    skip_if_not_installed("margins")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("brglm2")
    withr_library("margins")
    withr_library("emmeans")
    withr_library("broom")

    # brglm2::brglm_fit vs. margins vs. emtrends
    data("endometrial", package = "brglm2", envir = environment())
    dat <- endometrial
    model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = dat)
    model <- update(model, method = "brglm_fit") # probably breaks get_data from environemnt

    # margins
    mar <- margins(model)
    mfx <- slopes(model, newdata = dat)
    slo <- slopes(model, newdata = dat)
    expect_s3_class(slo, "slopes")
    expect_true(all(abs(mfx$estimate - mar$dydx) < 0.1))
    # emtrends
    em <- emtrends(model, ~PI, "PI", at = list(PI = 15, EH = 2, NV = 0))
    em <- tidy(em)
    mfx <- slopes(
        model,
        variables = "PI",
        newdata = datagrid(PI = 15, EH = 2, NV = 0),
        type = "link"
    )
    expect_equal(mfx$estimate, em$PI.trend, ignore_attr = TRUE)
    expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)

    # predictions: brglm2::brglm_fit: no validity
    data("endometrial", package = "brglm2", envir = environment())
    dat <- endometrial
    model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = dat)
    model <- update(model, method = "brglm_fit")
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(endometrial))
    expect_s3_class(pred1, "predictions")
    expect_equal(nrow(pred1), nrow(endometrial), ignore_attr = TRUE)
    expect_s3_class(pred2, "predictions")
    expect_equal(nrow(pred2), 6, ignore_attr = TRUE)

    # brmultinom: no validity
    z <- get_dataset("housing", package = "MASS")
    mod <- brmultinom(Sat ~ Infl + Type + Cont, weights = Freq, data = z, type = "ML", ref = 1)
    slo <- slopes(mod, type = "probs")
    expect_s3_class(slo, "slopes")
    pre <- predictions(mod, type = "probs")
    expect_s3_class(pre, "predictions")

    # bracl: no validity
    data("stemcell", package = "brglm2")
    dat <- stemcell
    dat$religion <- as.numeric(dat$religion)
    mod <- bracl(
        research ~ as.numeric(religion) + gender,
        weights = frequency,
        data = dat,
        type = "ML"
    )
    pre <- predictions(mod, type = "probs")
    expect_s3_class(pre, "predictions")
    slo <- slopes(mod, type = "probs")
    expect_s3_class(slo, "slopes")

    # brglm2::brglm_fit vs. margins
    tmp <- data.frame(
        freq = c(15, 16, 16, 27, 33, 20, 21, 18, 26, 41, 38, 27, 29, 21, 33, 60, 41, 42),
        dose = rep(c(0, 10, 33, 100, 333, 1000), 3),
        observation = rep(1:3, each = 6)
    )
    model <- brnb(
        freq ~ dose + log(dose + 10),
        data = tmp,
        link = "log",
        transformation = "inverse",
        type = "ML"
    )
    slo <- slopes(model, newdata = tmp)
    expect_s3_class(slo, "slopes")
    mfx <- suppressWarnings(slopes(model))
    mar <- suppressWarnings(margins(model))
    expect_true(all(abs(mfx$estimate - mar$dydx) < 0.1))
})
