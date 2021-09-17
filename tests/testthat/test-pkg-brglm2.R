skip_if_not_installed("brglm2")
requiet("brglm2")

test_that("brglm2::brglm_fit no validity check", {
    data("endometrial", package = "brglm2")
    model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = endometrial)
    model <- update(model, method = "brglm_fit")
    expect_mfx(model)
})

test_that("brglm2::brglm_fit no validity check", {
    salmonella <- data.frame(freq = c(15, 16, 16, 27, 33, 20, 21, 18, 26, 41, 38, 27, 29, 21, 33, 60, 41, 42),
                             dose = rep(c(0, 10, 33, 100, 333, 1000), 3),
                             observation = rep(1:3, each = 6))
    salmonella_fm <- freq ~ dose + log(dose + 10)
    model <- brnb(salmonella_fm, data = salmonella, link = "log", transformation = "inverse", type = "ML")
    suppressWarnings(expect_mfx(model, n_unique = 6))
})
