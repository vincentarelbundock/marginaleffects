test_that("multiple prediction types", {
    skip_if_not_installed("pscl")
    library("pscl")
    data("bioChemists", package = "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    mfx <- marginaleffects(model, prediction_type = c("response", "zero"))
    expect_true(all(c("response", "zero") %in% mfx$type))
})
