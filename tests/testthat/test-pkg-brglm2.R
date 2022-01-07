requiet("brglm2")
requiet("margins")
requiet("emmeans")

test_that("brglm2::brglm_fit vs. margins vs. emtrends", {
    data("endometrial", package = "brglm2")
    model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = endometrial)
    model <- update(model, method = "brglm_fit")
    # margins
    mar <- margins(model)
    mfx <- marginaleffects(model)
    expect_marginaleffects(model)
    expect_true(test_against_margins(mar, mfx))
    # emtrends
    em <- emtrends(model, ~PI, "PI", at = list(PI = 15, EH = 2, NV = 0))
    em <- tidy(em)
    mfx <- marginaleffects(model,
                           variables = "PI",
                           newdata = datagrid(PI = 15, EH = 2, NV = 0), 
                           type = "link")
    expect_equal(mfx$dydx, em$PI.trend)
    expect_equal(mfx$std.error, em$std.error, tolerance = .00001)
})

test_that("brglm2::brglm_fit vs. margins", {
    salmonella <- data.frame(freq = c(15, 16, 16, 27, 33, 20, 21, 18, 26, 41, 38, 27, 29, 21, 33, 60, 41, 42),
                             dose = rep(c(0, 10, 33, 100, 333, 1000), 3),
                             observation = rep(1:3, each = 6))
    salmonella_fm <- freq ~ dose + log(dose + 10)
    model <- brnb(salmonella_fm, data = salmonella, link = "log", transformation = "inverse", type = "ML")
    suppressWarnings(expect_marginaleffects(model, n_unique = 6))
    mfx <- suppressWarnings(marginaleffects(model))
    mar <- suppressWarnings(margins(model))
    expect_true(test_against_margins(mar, mfx))
})

test_that("predictions: brglm2::brglm_fit: no validity", {
    data("endometrial", package = "brglm2")
    model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = endometrial)
    model <- update(model, method = "brglm_fit")
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(endometrial))
    expect_predictions(pred1, n_row = nrow(endometrial))
    expect_predictions(pred2, n_row = 6)
})


test_that("brmultinom: no validity", {
    data("housing", package = "MASS")
    mod <- brmultinom(Sat ~ Infl + Type + Cont, weights = Freq,
                      data = housing, type = "ML", ref = 1)
    expect_marginaleffects(mod, type = "probs")
    expect_predictions(predictions(mod, type = "probs"))
})

