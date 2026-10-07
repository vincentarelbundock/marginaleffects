source("helpers.R")
using("marginaleffects")
requiet("fixest")

# Each analytic standard error must match the finite-difference fallback.
numeric_se <- function(expr) {
    old <- options(marginaleffects_analytic_jacobian = FALSE)
    on.exit(options(old))
    eval.parent(substitute(expr))$std.error
}

dat <- base_stagg
dat$D <- dat$year >= dat$year_treated
dat$yb <- dat$y > 0
late <- subset(dat, year > 5) # i() levels 2 to 5 are absent


# feols with fixed effects and i() interactions
mod <- feols(y ~ D + D:x1 + i(year, x1, ref = 1) | id + year, data = dat, vcov = ~id)
cmp <- avg_comparisons(mod, variables = "D")
expect_equal(components(cmp, "jacobian_method"), "analytic")
expect_equivalent(cmp$std.error, numeric_se(avg_comparisons(mod, variables = "D")), tolerance = 1e-6)


# i() omits columns for levels absent from newdata; the analytic path must not
# silently fall back
cmp <- avg_comparisons(mod, variables = "D", newdata = late)
expect_equal(components(cmp, "jacobian_method"), "analytic")
expect_equivalent(cmp$std.error, numeric_se(avg_comparisons(mod, variables = "D", newdata = late)), tolerance = 1e-6)


# varying slopes are absorbed too
mod_vs <- feols(y ~ D + D:x1 | id[year] + year, data = dat, vcov = ~id)
cmp <- avg_comparisons(mod_vs, variables = "D")
expect_equal(components(cmp, "jacobian_method"), "analytic")
expect_equivalent(cmp$std.error, numeric_se(avg_comparisons(mod_vs, variables = "D")), tolerance = 1e-6)


# nonlinear fixest models without fixed effects
logit <- feglm(yb ~ D * x1, data = dat, family = binomial())
cmp <- avg_comparisons(logit, variables = "D")
expect_equal(components(cmp, "jacobian_method"), "analytic")
expect_equivalent(cmp$std.error, numeric_se(avg_comparisons(logit, variables = "D")), tolerance = 1e-5)


# IV is not eligible
iv <- feols(y ~ 1 | id + year | D ~ x1, data = dat)
cmp <- avg_comparisons(iv, variables = "D")
expect_equal(components(cmp, "jacobian_method"), "numeric")


# unconditional variance reuses the analytic effect Jacobian
V <- vcovUnconditional(cluster = ~id)
cmp <- avg_comparisons(mod, variables = "D", newdata = late, vcov = V)
expect_equal(components(cmp, "jacobian_method"), "analytic")
expect_equivalent(
    cmp$std.error,
    numeric_se(avg_comparisons(mod, variables = "D", newdata = late, vcov = V)),
    tolerance = 1e-6
)

mod_lm <- lm(y ~ D * x1, data = dat)
p <- avg_predictions(mod_lm, by = "D", vcov = "unconditional")
expect_equal(components(p, "jacobian_method"), "analytic")
expect_equivalent(
    p$std.error,
    numeric_se(avg_predictions(mod_lm, by = "D", vcov = "unconditional")),
    tolerance = 1e-6
)
