test_that("hurdle: no validity check", {
    library("pscl")
    data("bioChemists", package = "pscl")
    dat <- bioChemists
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    mfx1 <- marginaleffects(model, prediction_type = "response")
    mfx2 <- marginaleffects(model, prediction_type = "zero")
    mfx1 <- tidy(mfx1)
    mfx2 <- tidy(mfx2)
    expect_s3_class(mfx1, "data.frame")
    expect_s3_class(mfx2, "data.frame")
})
