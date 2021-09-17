test_that("multiple prediction types", {
    skip_if_not_installed("pscl")
    requiet("pscl")
    data("bioChemists", package = "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    mfx <- marginaleffects(model, predict_type = c("response", "zero"))
    expect_true(all(c("response", "zero") %in% mfx$type))
})


test_that("predicted values are added to the data.frame", {
    mod <- glm(am ~ mpg * wt, data = mtcars, family = binomial)
    mfx <- marginaleffects(mod, predict_type = "response")
    expect_true("response" %in% mfx$type)
    mfx <- marginaleffects(mod, predict_type = c("response", "link"))
    expect_true(all(c("response", "link") %in% mfx$type))
})
