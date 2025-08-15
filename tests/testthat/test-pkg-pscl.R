test_that("pscl package: hurdle model set_coef", {
    skip_if_not_installed("pscl")

    withr_library("pscl")

    # hurdle: set_coef
    data("bioChemists", package = "pscl")
    mod1 <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    beta <- stats::setNames(rep(0, length(coef(mod1))), names(coef(mod1)))
    mod2 <- set_coef(mod1, beta)
    expect_true(all(coef(mod1) != coef(mod2)))
})

test_that("pscl package: hurdle model marginal effects", {
    skip_if_not_installed("pscl")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")
    skip_if_not_installed("margins")

    withr_library("pscl")
    withr_library("emmeans")
    withr_library("broom")
    withr_library("margins")

    # hurdle: marginaleffects vs margins vs emtrends
    data("bioChemists", package = "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    mfx1 <- avg_slopes(model, type = "response")
    mfx2 <- avg_slopes(model, type = "zero")
    expect_false(any(mfx1$estimate == 0))
    expect_false(any(mfx2$estimate == 0))
    expect_false(any(mfx1$std.error == 0))
    expect_false(any(mfx2$std.error == 0))
    expect_s3_class(mfx1, "data.frame")
    expect_s3_class(mfx2, "data.frame")

    # emtrends
    em <- emtrends(model, ~phd, "phd", at = list(fem = "Men", phd = 2), df = Inf)
    em <- tidy(em)
    mfx <- slopes(model, newdata = datagrid(fem = "Men", phd = 2), variables = "phd")
    expect_equal(mfx$estimate, em$phd.trend, tolerance = .01, ignore_attr = TRUE)
    # standard errors do not match
    # expect_equal(mfx$std.error, em$std.error, ignore_attr = TRUE)

    # margins: standard errors are not supported (all zeros)
    res <- slopes(model, newdata = head(bioChemists, 2))
    mar <- margins(model, data = head(bioChemists, 2), unit_ses = TRUE)
    expect_equal(res$estimate[res$term == "phd"], as.numeric(mar$dydx_phd), tolerance = .0001, ignore_attr = TRUE)
    expect_equal(res$estimate[res$term == "fem"], as.numeric(mar$dydx_femWomen), tolerance = .00001, ignore_attr = TRUE)
})

test_that("pscl package: hurdle model multi-level regressor", {
    skip_if_not_installed("pscl")

    withr_library("pscl")

    # bugs stay dead: hurdle with multi-level regressor
    data("bioChemists", package = "pscl")
    tmp <- bioChemists
    tmp$fem <- as.character(tmp$fem)
    tmp$fem[sample(1:nrow(tmp), 300)] <- "Other"
    tmp$fem <- as.factor(tmp$fem)
    model <- hurdle(art ~ phd + fem | ment, data = tmp, dist = "negbin")
    slo <- slopes(model)
    expect_s3_class(slo, "slopes")
})

test_that("pscl package: zeroinfl vs Stata comparison", {
    skip_if_not_installed("pscl")

    withr_library("pscl")

    tol_se <- 0.001

    # marginaleffects: zeroinfl vs. Stata vs. emtrends
    data("bioChemists", package = "pscl")
    model <- zeroinfl(art ~ kid5 + phd | ment, dist = "negbin", data = bioChemists)

    # stata
    stata <- readRDS(test_path("stata/stata.rds"))$pscl_zeroinfl_01
    mfx <- merge(avg_slopes(model), stata)
    slo <- slopes(model)
    expect_s3_class(slo, "slopes")
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = 1e-3, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = tol_se, ignore_attr = TRUE)
})

test_that("pscl package: zeroinfl vs emtrends", {
    skip_if_not_installed("pscl")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("pscl")
    withr_library("emmeans")
    withr_library("broom")

    # emtrends
    data("bioChemists", package = "pscl")
    model <- zeroinfl(art ~ kid5 + phd | ment, dist = "negbin", data = bioChemists)
    mfx <- slopes(model, variables = "phd", newdata = datagrid(kid5 = 2, ment = 7, phd = 2))
    em <- emtrends(model, ~phd, "phd", at = list(kid5 = 2, ment = 7, phd = 2))
    em <- tidy(em)
    expect_equal(mfx$estimate, em$phd.trend, tolerance = .0001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, em$std.error, tolerance = .01, ignore_attr = TRUE)
})

test_that("pscl package: zeroinfl vs margins", {
    skip_if_not_installed("pscl")
    skip_if_not_installed("margins")

    withr_library("pscl")
    withr_library("margins")

    # margins: does not support standard errors (all zeros)
    data("bioChemists", package = "pscl")
    model <- zeroinfl(art ~ kid5 + phd | ment, dist = "negbin", data = bioChemists)
    mar <- margins(model, data = head(bioChemists), unit_ses = TRUE)
    mfx <- avg_slopes(model, variables = c("kid5", "phd", "ment"), newdata = head(bioChemists))
    expect_equal(sort(summary(mar)$AME), sort(mfx$estimate), tolerance = 1e-3, ignore_attr = TRUE)
})

test_that("pscl package: zeroinfl predictions", {
    skip_if_not_installed("pscl")

    withr_library("pscl")

    # marginaleffects: zeroinfl: no validity
    data("bioChemists", package = "pscl")
    model <- zeroinfl(art ~ kid5 + phd | ment, dist = "negbin", data = bioChemists)
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(bioChemists))
    expect_s3_class(pred1, "predictions")
    expect_s3_class(pred2, "predictions")
    expect_equal(nrow(pred2), 6, ignore_attr = TRUE)
})

test_that("pscl package: zeroinfl marginal means", {
    skip_if_not_installed("pscl")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("pscl")
    withr_library("emmeans")
    withr_library("broom")

    # zeroinfl: marginalmeans vs. emmeans
    data("bioChemists", package = "pscl")
    model <- zeroinfl(art ~ kid5 + phd + mar | ment, dist = "negbin", data = bioChemists)
    # response
    mm <- predictions(model, by = "mar", newdata = datagrid(grid_type = "balanced")) |> dplyr::arrange(mar)
    em <- tidy(emmeans(model, specs = "mar", df = Inf))
    expect_equal(mm$estimate, em$estimate, tolerance = 0.01, ignore_attr = TRUE)
    expect_equal(mm$std.error, em$std.error, tolerance = .01, ignore_attr = TRUE)
})
