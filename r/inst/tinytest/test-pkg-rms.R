source("helpers.R")
using("marginaleffects")

requiet("polspline")
requiet("rms")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
mod_simple <- lrm(am ~ mpg + wt, data = mtcars)
expect_slopes(mod_simple, newdata = mtcars)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

# predictrms(type = "x") handles RMS transformations but omits intercepts.
# The package method restores them and aligns columns with the coefficients.
dat_analytic <- mtcars
dat_analytic$cyl <- factor(dat_analytic$cyl)

mod_ols_analytic <- rms::ols(
    mpg ~ rms::rcs(wt, 4) + cyl,
    data = dat_analytic,
    x = TRUE,
    y = TRUE
)
newdata <- dat_analytic[1:12, , drop = FALSE]
X <- marginaleffects:::get_model_matrix(mod_ols_analytic, newdata)
expect_equivalent(
    drop(X %*% coef(mod_ols_analytic)),
    as.numeric(predict(mod_ols_analytic, newdata = newdata, type = "lp"))
)

mod_lrm_analytic <- rms::lrm(
    am ~ rms::rcs(wt, 4) + cyl,
    data = dat_analytic,
    x = TRUE,
    y = TRUE
)
X <- marginaleffects:::get_model_matrix(mod_lrm_analytic, newdata)
expect_equivalent(
    drop(X %*% coef(mod_lrm_analytic)),
    as.numeric(predict(mod_lrm_analytic, newdata = newdata, type = "lp"))
)
cmp_analytic <- avg_comparisons(
    mod_lrm_analytic,
    variables = "wt",
    type = "fitted"
)
old_option <- options(marginaleffects_analytic_jacobian = FALSE)
cmp_fallback <- avg_comparisons(
    mod_lrm_analytic,
    variables = "wt",
    type = "fitted"
)
options(old_option)
expect_equivalent(cmp_analytic$estimate, cmp_fallback$estimate)
expect_equivalent(
    components(cmp_analytic, "jacobian"),
    components(cmp_fallback, "jacobian"),
    tolerance = 1e-5
)

# Ordinal lrm() has several intercepts. Link-scale prediction selects the RMS
# reference intercept and remains linear; fitted probabilities are multi-output
# and must remain on the fallback path.
dat_ordinal <- dat_analytic
dat_ordinal$gear <- ordered(dat_ordinal$gear)
mod_lrm_ordinal <- rms::lrm(
    gear ~ rms::rcs(wt, 4) + cyl,
    data = dat_ordinal,
    x = TRUE,
    y = TRUE
)
X_ordinal <- marginaleffects:::get_model_matrix(mod_lrm_ordinal, newdata)
expect_equivalent(
    drop(X_ordinal %*% coef(mod_lrm_ordinal)),
    as.numeric(predict(mod_lrm_ordinal, newdata = newdata, type = "lp"))
)
cmp_ordinal_analytic <- suppressWarnings(avg_comparisons(
    mod_lrm_ordinal,
    variables = "wt",
    type = "lp"
))
old_option <- options(marginaleffects_analytic_jacobian = FALSE)
cmp_ordinal_fallback <- suppressWarnings(avg_comparisons(
    mod_lrm_ordinal,
    variables = "wt",
    type = "lp"
))
options(old_option)
expect_equivalent(
    components(cmp_ordinal_analytic, "jacobian"),
    components(cmp_ordinal_fallback, "jacobian"),
    tolerance = 1e-5
)
expect_null(marginaleffects:::get_jacobian_analytic(
    mod_lrm_ordinal,
    type = "fitted"
))

# lmr: marginaleffects vs emtrends
model <- rms::lrm(am ~ mpg, mtcars)
expect_slopes(model, type = "lp", n_unique = 1)
mfx <- slopes(model, newdata = data.frame(mpg = 30), type = "lp", eps = 1 / 1000 * diff(range(mtcars$mpg)))
em <- emtrends(model, ~mpg, "mpg", at = list(mpg = 30))
em <- tidy(em)
expect_equivalent(mfx$estimate, em$mpg.trend)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .0001)


# predictions: rms: no validity
model <- rms::lrm(am ~ mpg, mtcars)
pred1 <- predictions(model, type = "lp")
pred2 <- predictions(model, type = "lp", newdata = head(mtcars))
expect_predictions(model, n_row = 32)
expect_predictions(model, n_row = 6)


# comparisons
mod <- ols(mpg ~ hp, mtcars)
c1 <- comparisons(mod, type = "lp")
expect_inherits(c1, "comparisons")

mod <- lrm(am ~ hp, mtcars)
c1 <- comparisons(mod, type = "fitted")
c2 <- comparisons(mod, type = "lp")
expect_inherits(c1, "comparisons")
expect_inherits(c2, "comparisons")



mod <- orm(cyl ~ hp, mtcars)
c1 <- comparisons(mod, type = "fitted")
c2 <- comparisons(mod, type = "lp")
c3 <- comparisons(mod, type = "mean")
expect_inherits(c1, "comparisons")
expect_inherits(c2, "comparisons")

expect_error(comparisons(mod, vcov = "HC3"), pattern = "supported")


# Issue #1428
requiet("tibble")
data <- tibble::tibble(
    y = rbinom(100, 1, .4),
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rep(c("A", "B"), 50)
)
f <- lrm(y ~ ., data = data)
p <- suppressWarnings(get_predict(f))
expect_inherits(p, "data.frame")
expect_equal(dim(p), c(100, 1))
expect_warning(get_predict(f), pattern = "Converting.*tibble")


mod <- lrm(cyl ~ hp, mtcars)
c1 <- comparisons(mod, type = "fitted")
c2 <- comparisons(mod, type = "lp")
expect_inherits(c1, "comparisons")
expect_inherits(c2, "comparisons")


# Issue 1600
dat <- mtcars
dat$gear_ord <- factor(dat$gear, ordered = TRUE)
mod <- orm(mpg ~ gear_ord + hp * wt, data = dat, x = TRUE, y = TRUE)
expect_warning(
    avg_comparisons(mod, variables = "gear_ord", type = "lp"),
    pattern = "Ordered factors sometimes cause issues with `rms` models")
