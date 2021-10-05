skip_if_not_installed("pscl")
requiet("pscl")


### marginaleffects

test_that("marginaleffects: hurdle: set_coef", {
    data("bioChemists", package = "pscl")
    mod1 <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    beta <- stats::setNames(rep(0, length(coef(mod1))), names(coef(mod1)))
    mod2 <- set_coef(mod1, beta)
    expect_true(all(coef(mod1) != coef(mod2)))
})

test_that("marginaleffects: hurdle: no validity check", {
    data("bioChemists", package = "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    mfx1 <- marginaleffects(model, type = "response")
    mfx2 <- marginaleffects(model, type = "zero")
    mfx1 <- tidy(mfx1)
    mfx2 <- tidy(mfx2)
    expect_false(any(mfx1$estimate == 0))
    expect_false(any(mfx2$estimate == 0))
    expect_false(any(mfx1$std.error == 0))
    expect_false(any(mfx2$std.error == 0))
    expect_s3_class(mfx1, "data.frame")
    expect_s3_class(mfx2, "data.frame")
})

test_that("marginaleffects: zeroinfl vs. Stata", {
    data("bioChemists", package = "pscl")
    model <- zeroinfl(art ~ kid5 + phd | ment,
                      dist = "negbin",
                      data = bioChemists)
    stata <- readRDS(test_path("stata/stata.rds"))$pscl_zeroinfl_01
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_marginaleffects(model)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .00001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
    mfx <- marginaleffects(model)
})


### predictions

test_that("marginaleffects: zeroinfl: no validity", {
    data("bioChemists", package = "pscl")
    model <- zeroinfl(art ~ kid5 + phd | ment,
                      dist = "negbin",
                      data = bioChemists)
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(bioChemists))
    expect_predictions(pred1, n_row = 1, se = FALSE)
    expect_predictions(pred2, n_row = 6, se = FALSE)
})


### marginalmeans

test_that("marginalmeans: zeroinfl: no validity", {
    data("bioChemists", package = "pscl")
    model <- zeroinfl(art ~ kid5 + phd + mar | ment,
                      dist = "negbin",
                      data = bioChemists)
    mm <- marginalmeans(model)
    expect_marginalmeans(mm, se = FALSE)
})
