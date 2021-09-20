test_that("multiple prediction types", {
    skip_if_not_installed("pscl")
    requiet("pscl")
    data("bioChemists", package = "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    mfx <- marginaleffects(model, type = c("response", "zero"))
    expect_true(all(c("response", "zero") %in% mfx$type))
})


test_that("predicted values are added to the data.frame", {
    mod <- glm(am ~ mpg * wt, data = mtcars, family = binomial)
    mfx <- marginaleffects(mod, type = "response")
    expect_true("response" %in% mfx$type)
    mfx <- marginaleffects(mod, type = c("response", "link"))
    expect_true(all(c("response", "link") %in% mfx$type))
})


test_that("multiple prediction types", {
    skip_if_not_installed("pscl")
    requiet("pscl")
    data("bioChemists", package = "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    mfx <- marginaleffects(model, type = c("response", "zero"))
    expect_true(all(c("response", "zero") %in% mfx$type))
})


test_that("error: multivariate", {
    skip_if_not_installed("pscl")
    requiet("pscl")
    data("bioChemists", package = "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    expect_error(mfx <- marginaleffects(model, type = "prob"), regexp = "feature request")
})


test_that("error: character", {
    skip_if_not_installed("MASS")
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    mod <- MASS::polr(factor(y) ~ x1 + x2, data = dat)
    expect_error(get_predict(mod, type = "class"), regexp = "differentiated")
})
