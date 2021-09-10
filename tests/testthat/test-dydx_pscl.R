skip_if_not_installed("pscl")

test_that("hurdle: set_coef", {
    library("pscl")
    data("bioChemists", package = "pscl")
    mod1 <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    beta <- stats::setNames(rep(0, length(coef(mod1))), names(coef(mod1)))
    mod2 <- set_coef(mod1, beta)
    expect_true(all(coef(mod1) != coef(mod2)))
})


test_that("hurdle: no validity check", {
    library("pscl")
    data("bioChemists", package = "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    mfx1 <- marginaleffects(model, prediction_type = "response")
    mfx2 <- marginaleffects(model, prediction_type = "zero")
    mfx1 <- tidy(mfx1)
    mfx2 <- tidy(mfx2)
    expect_false(any(mfx1$estimate == 0))
    expect_false(any(mfx2$estimate == 0))
    expect_false(any(mfx1$std.error == 0))
    expect_false(any(mfx2$std.error == 0))
    expect_s3_class(mfx1, "data.frame")
    expect_s3_class(mfx2, "data.frame")
})
