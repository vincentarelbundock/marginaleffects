skip_if_not_installed("brglm2")
requiet("brglm2")
requiet("margins")

test_that("brglm2::brglm_fit vs. margins", {
    data("endometrial", package = "brglm2")
    model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = endometrial)
    model <- update(model, method = "brglm_fit")
    mar <- margins(model)
    mfx <- marginaleffects(model)
    expect_marginaleffects(model)
    expect_true(test_against_margins(mar, mfx))
})

test_that("brglm2::brglm_fit no validity check", {
    salmonella <- data.frame(freq = c(15, 16, 16, 27, 33, 20, 21, 18, 26, 41, 38, 27, 29, 21, 33, 60, 41, 42),
                             dose = rep(c(0, 10, 33, 100, 333, 1000), 3),
                             observation = rep(1:3, each = 6))
    salmonella_fm <- freq ~ dose + log(dose + 10)
    model <- brnb(salmonella_fm, data = salmonella, link = "log", transformation = "inverse", type = "ML")
    suppressWarnings(expect_marginaleffects(model, n_unique = 6))
})

test_that("predictions: brglm2::brglm_fit: no validity", {
    data("endometrial", package = "brglm2")
    model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = endometrial)
    model <- update(model, method = "brglm_fit")
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(endometrial))
    expect_predictions(pred1, n_row = 1, se = TRUE)
    expect_predictions(pred2, n_row = 6, se = TRUE)
})
