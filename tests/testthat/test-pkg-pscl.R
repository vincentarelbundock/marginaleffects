testthat::skip_if_not_installed("pscl")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")
testthat::skip_if_not_installed("margins")
requiet("pscl")
requiet("emmeans")
requiet("broom")
requiet("margins")
tol <- 0.0001
tol_se <- 0.001

# Basic expectation tests
data("bioChemists", package = "pscl")
mod_simple <- pscl::hurdle(art ~ fem + phd, data = bioChemists)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

### marginaleffects

# hurdle: set_coef
data("bioChemists", package = "pscl")
mod1 <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
beta <- stats::setNames(rep(0, length(coef(mod1))), names(coef(mod1)))
mod2 <- set_coef(mod1, beta)
expect_true(all(coef(mod1) != coef(mod2)))


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


# bugs stay dead: hurdle with multi-level regressor
data("bioChemists", package = "pscl")
tmp_pscl <- bioChemists
tmp_pscl$fem <- as.character(tmp_pscl$fem)
tmp_pscl$fem[sample(1:nrow(tmp_pscl), 300)] <- "Other"
tmp_pscl$fem <- as.factor(tmp_pscl$fem)
model <- hurdle(art ~ phd + fem | ment, data = tmp_pscl, dist = "negbin")
expect_slopes2(model)


# marginaleffects: zeroinfl vs. Stata vs. emtrends
data("bioChemists", package = "pscl")
model <- zeroinfl(art ~ kid5 + phd | ment, dist = "negbin", data = bioChemists)

# stata
stata <- readRDS(testing_path("stata/stata.rds"))$pscl_zeroinfl_01
mfx <- merge(avg_slopes(model), stata)
expect_slopes2(model)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = 1e-3, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = tol_se, ignore_attr = TRUE)

# emtrends
mfx <- slopes(model, variables = "phd", newdata = datagrid(kid5 = 2, ment = 7, phd = 2))
em <- emtrends(model, ~phd, "phd", at = list(kid5 = 2, ment = 7, phd = 2))
em <- tidy(em)
expect_equal(mfx$estimate, em$phd.trend, tolerance = .0001, ignore_attr = TRUE)
expect_equal(mfx$std.error, em$std.error, tolerance = .01, ignore_attr = TRUE)

# margins: does not support standard errors (all zeros)
mar <- margins(model, data = head(bioChemists), unit_ses = TRUE)
mfx <- avg_slopes(model, variables = c("kid5", "phd", "ment"), newdata = head(bioChemists))
expect_equal(sort(summary(mar)$AME), sort(mfx$estimate), tolerance = 1e-3, ignore_attr = TRUE)


### predictions
# marginaleffects: zeroinfl: no validity
data("bioChemists", package = "pscl")
model <- zeroinfl(art ~ kid5 + phd | ment, dist = "negbin", data = bioChemists)
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(bioChemists))
expect_predictions2(model)
expect_predictions2(model, newdata = head(bioChemists), n_row = 6)


### marginalmeans

# zeroinfl: marginalmeans vs. emmeans
data("bioChemists", package = "pscl")
model <- zeroinfl(art ~ kid5 + phd + mar | ment, dist = "negbin", data = bioChemists)
# response
mm <- predictions(model,
    by = "mar",
    newdata = datagrid(grid_type = "balanced", FUN_integer = mean)) |>
    sort_by(~mar)
em <- tidy(emmeans(model, specs = "mar", df = Inf))
expect_equal(mm$estimate, em$estimate, tolerance = 0.01, ignore_attr = TRUE)
expect_equal(mm$std.error, em$std.error, tolerance = .01, ignore_attr = TRUE)
