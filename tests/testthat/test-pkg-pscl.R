requiet("pscl")
requiet("emmeans")
requiet("broom")
requiet("margins")
tol <- 0.0001
tol_se <- 0.001

### marginaleffects

test_that("hurdle: set_coef", {
    data("bioChemists", package = "pscl")
    mod1 <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    beta <- stats::setNames(rep(0, length(coef(mod1))), names(coef(mod1)))
    mod2 <- set_coef(mod1, beta)
    expect_true(all(coef(mod1) != coef(mod2)))
})

test_that("hurdle: marginaleffects vs margins vs emtrends", {
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

    # emtrends
    em <- emtrends(model, ~phd, "phd", at = list(fem = "Men", phd = 2))
    em <- tidy(em)
    mfx <- marginaleffects(model, newdata = datagrid(fem = "Men", phd = 2), variables = "phd")
    expect_equal(mfx$dydx, em$phd.trend, tolerance = .001)
    # standard errors do not match
    # expect_equal(mfx$std.error, em$std.error)

    # margins: standard errors are not supported (all zeros)
    res <- marginaleffects(model, newdata = head(bioChemists, 2))
    mar <- margins(model, data = head(bioChemists, 2), unit_ses = TRUE)
    expect_equal(res$dydx[1:2], mar$dydx_phd, tolerance = .00001, ignore_attr = TRUE)
    expect_equal(res$dydx[3:4], mar$dydx_femWomen, tolerance = .00001, ignore_attr = TRUE)
})


test_that("bugs stay dead: hurdle with multi-level regressor", {
    data("bioChemists", package = "pscl")
    tmp <- bioChemists
    tmp$fem <- as.character(tmp$fem)
    tmp$fem[sample(1:nrow(tmp), 300)] <- "Other"
    tmp$fem <- as.factor(tmp$fem)
    model <- hurdle(art ~ phd + fem | ment, data = tmp, dist = "negbin")
    expect_marginaleffects(model)
})


test_that("marginaleffects: zeroinfl vs. Stata vs. emtrends", {
    data("bioChemists", package = "pscl")
    model <- zeroinfl(art ~ kid5 + phd | ment,
                      dist = "negbin",
                      data = bioChemists)

    # stata
    stata <- readRDS(test_path("stata/stata.rds"))$pscl_zeroinfl_01
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_marginaleffects(model)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = tol)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = tol_se)

    # emtrends
    mfx <- marginaleffects(model, variables = "phd", newdata = datagrid(kid5 = 2, ment = 7, phd = 2))
    em <- emtrends(model, ~phd, "phd", at = list(kid5 = 2, ment = 7, phd = 2))
    em <- tidy(em)
    expect_equal(mfx$dydx, em$phd.trend, tolerance = .0001)
    expect_equal(mfx$std.error, em$std.error, tolerance = .01)

    # margins: does not support standard errors (all zeros)
    mar <- margins(model, data = head(bioChemists), unit_ses = TRUE)
    mfx <- marginaleffects(model, newdata = head(bioChemists))
    expect_true(test_against_margins(mfx, mar, se = FALSE, verbose = TRUE, tolerance = 0.001))
})


### predictions
test_that("marginaleffects: zeroinfl: no validity", {
    data("bioChemists", package = "pscl")
    model <- zeroinfl(art ~ kid5 + phd | ment,
                      dist = "negbin",
                      data = bioChemists)
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(bioChemists))
    expect_predictions(pred1)
    expect_predictions(pred2, n_row = 6)
})


### marginalmeans

test_that("zeroinfl: marginalmeans vs. emmeans", {
    data("bioChemists", package = "pscl")
    model <- zeroinfl(art ~ kid5 + phd + mar | ment,
                      dist = "negbin",
                      data = bioChemists)
    mm <- marginalmeans(model)
    expect_marginalmeans(mm)
    # response
    mm <- tidy(marginalmeans(model, type = "response"))
    em <- tidy(emmeans(model, specs = "mar"))
    expect_equal(mm$estimate, em$estimate)
    expect_equal(mm$std.error, em$std.error, tolerance = .001)
})
