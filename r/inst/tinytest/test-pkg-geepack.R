source("helpers.R")
using("marginaleffects")

requiet("geepack")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
data("dietox", package = "geepack")
mod_simple <- geepack::geeglm(Weight ~ Time + Cu, id = Pig, data = dietox)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

# geeglm inherits predict.glm(), including its model-matrix and inverse-link
# behavior. An all-zero stored offset is the no-offset case for this class.
dietox$Cu <- factor(dietox$Cu)
mod_analytic <- suppressWarnings(geepack::geeglm(
    Weight ~ Time * Cu + splines::ns(Start, 3),
    id = Pig,
    data = dietox,
    family = poisson("log")
))
newdata <- dietox[1:12, , drop = FALSE]
X <- marginaleffects:::get_model_matrix(mod_analytic, newdata)
expect_equivalent(
    drop(X %*% coef(mod_analytic)),
    as.numeric(predict(mod_analytic, newdata = newdata, type = "link"))
)
cmp_analytic <- avg_comparisons(
    mod_analytic,
    variables = "Time",
    type = "response"
)
J_analytic <- components(cmp_analytic, "jacobian")
old_option <- options(marginaleffects_analytic_jacobian = FALSE)
cmp_fallback <- avg_comparisons(
    mod_analytic,
    variables = "Time",
    type = "response"
)
options(old_option)
expect_equivalent(cmp_analytic$estimate, cmp_fallback$estimate)
expect_equivalent(
    J_analytic,
    components(cmp_fallback, "jacobian"),
    tolerance = 1e-5
)

# geeglm stores zero offsets even without an offset term, but an explicit
# offset—zero or otherwise—must remain ineligible for the matrix shortcut.
mod_offset <- suppressWarnings(geepack::geeglm(
    Weight ~ Time + offset(log(Start)),
    id = Pig,
    data = dietox,
    family = poisson("log")
))
expect_true(marginaleffects:::model_has_effective_offset(mod_offset))
cmp_offset <- avg_comparisons(
    mod_offset,
    variables = "Time",
    type = "response"
)
old_option <- options(marginaleffects_analytic_jacobian = FALSE)
cmp_offset_fallback <- avg_comparisons(
    mod_offset,
    variables = "Time",
    type = "response"
)
options(old_option)
expect_equivalent(
    components(cmp_offset, "jacobian"),
    components(cmp_offset_fallback, "jacobian"),
    tolerance = 1e-5
)

# Stata does not replicate coefficients exactly:
# xtset Pig Time
# xtgee Weight i.Cu, family(poisson) link(identity) corr(ar 1)

# geepack::geeglm: marginaleffects vs. emtrends
data(dietox, package = "geepack")
dietox$Cu <- as.factor(dietox$Cu)
mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
model <- suppressWarnings(geeglm(mf, data = dietox, id = Pig, family = poisson("identity"), corstr = "ar1"))
expect_slopes(model)
# emmeans
mfx <- slopes(model, variables = "Time", newdata = datagrid(Time = 10, Cu = "Cu000"), type = "link")
em <- suppressMessages(emtrends(model, ~Time, var = "Time", at = list(Time = 10, Cu = "Cu000")))
em <- tidy(em)
expect_equivalent(mfx$estimate, em$Time.trend, tolerance = .001)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .01)


# predictions: geepack::geeglm: no validity
data(dietox, package = "geepack")
dietox$Cu <- as.factor(dietox$Cu)
mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
model <- suppressWarnings(geeglm(mf, data = dietox, id = Pig, family = poisson("identity"), corstr = "ar1"))
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(dietox))
expect_predictions(model, n_row = nrow(dietox))
expect_predictions(model, newdata = head(dietox), n_row = 6)


# TODO: why no support for standard errors?
# marginalmeans: geepack::geeglm: vs. emmeans
data(dietox, package = "geepack")
dietox$Cu <- as.factor(dietox$Cu)
mf <- formula(Weight ~ Cu + Time + I(Time^2) + I(Time^3))
model <- suppressWarnings(geeglm(mf, data = dietox, id = Pig, family = poisson("identity"), corstr = "ar1"))

em <- tidy(emmeans::emmeans(model, ~Cu, df = Inf, at = list(Time = 10)), type = "response")
pr <- predictions(model, datagrid(Time = 10, Cu = unique))
expect_equivalent(em$estimate, pr$estimate)
expect_equivalent(em$std.error, pr$std.error, tolerance = 1e-5)

# TODO: not clear where `emmeans` holds the Time variable
# em <- emmeans::emmeans(model, ~Cu, type = "response", df = Inf)
# em <- data.frame(em)
# expect_equal(mm$estimate, em$emmean)
# expect_equal(mm$conf.low, em$asymp.LCL)
# expect_equal(mm$conf.high, em$asymp.UCL)
