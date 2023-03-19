source("helpers.R")
using("marginaleffects")

requiet("pscl")
requiet("emmeans")
requiet("broom")
requiet("margins")
tol <- 0.0001
tol_se <- 0.001

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
mfx1 <- slopes(model, type = "response")
mfx2 <- slopes(model, type = "zero")
mfx1 <- tidy(mfx1)
mfx2 <- tidy(mfx2)
expect_false(any(mfx1$estimate == 0))
expect_false(any(mfx2$estimate == 0))
expect_false(any(mfx1$std.error == 0))
expect_false(any(mfx2$std.error == 0))
expect_inherits(mfx1, "data.frame")
expect_inherits(mfx2, "data.frame")

# emtrends
em <- emtrends(model, ~phd, "phd", at = list(fem = "Men", phd = 2), df = Inf)
em <- tidy(em)
mfx <- slopes(model, newdata = datagrid(fem = "Men", phd = 2), variables = "phd")
expect_equivalent(mfx$estimate, em$phd.trend, tolerance = .01)
# standard errors do not match
# expect_equivalent(mfx$std.error, em$std.error)

# margins: standard errors are not supported (all zeros)
res <- slopes(model, newdata = head(bioChemists, 2))
mar <- margins(model, data = head(bioChemists, 2), unit_ses = TRUE)
expect_equivalent(res$estimate[5:6], as.numeric(mar$dydx_phd), tolerance = .0001)
expect_equivalent(res$estimate[1:2], as.numeric(mar$dydx_femWomen), tolerance = .00001)



# bugs stay dead: hurdle with multi-level regressor
data("bioChemists", package = "pscl")
tmp <- bioChemists
tmp$fem <- as.character(tmp$fem)
tmp$fem[sample(1:nrow(tmp), 300)] <- "Other"
tmp$fem <- as.factor(tmp$fem)
model <- hurdle(art ~ phd + fem | ment, data = tmp, dist = "negbin")
expect_slopes(model)



# marginaleffects: zeroinfl vs. Stata vs. emtrends
data("bioChemists", package = "pscl")
model <- zeroinfl(art ~ kid5 + phd | ment,
              dist = "negbin",
              data = bioChemists)

# stata
stata <- readRDS(testing_path("stata/stata.rds"))$pscl_zeroinfl_01
mfx <- merge(tidy(slopes(model)), stata)
expect_slopes(model)
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = 1e-3)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = tol_se)

# emtrends
mfx <- slopes(model, variables = "phd", newdata = datagrid(kid5 = 2, ment = 7, phd = 2))
em <- emtrends(model, ~phd, "phd", at = list(kid5 = 2, ment = 7, phd = 2))
em <- tidy(em)
expect_equivalent(mfx$estimate, em$phd.trend, tolerance = .0001)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .01)

# margins: does not support standard errors (all zeros)
mar <- margins(model, data = head(bioChemists), unit_ses = TRUE)
mfx <- slopes(model, variables = c("kid5", "phd", "ment"), newdata = head(bioChemists))
expect_equivalent(sort(summary(mar)$AME), sort(summary(mfx)$estimate), tolerance = 1e-3)


### predictions
# marginaleffects: zeroinfl: no validity
data("bioChemists", package = "pscl")
model <- zeroinfl(art ~ kid5 + phd | ment,
              dist = "negbin",
              data = bioChemists)
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(bioChemists))
expect_predictions(pred1)
expect_predictions(pred2, n_row = 6)



### marginalmeans

# zeroinfl: marginalmeans vs. emmeans
data("bioChemists", package = "pscl")
model <- zeroinfl(art ~ kid5 + phd + mar | ment,
              dist = "negbin",
              data = bioChemists)
mm <- marginal_means(model)
expect_marginal_means(mm)
# response
mm <- tidy(marginal_means(model))
em <- tidy(emmeans(model, specs = "mar", df = Inf))
expect_equivalent(mm$estimate, em$estimate, tol = 0.01)
expect_equivalent(mm$std.error, em$std.error, tolerance = .01)




rm(list = ls())