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


# feols with absorbed fixed effects reproduces lm() with dummies exactly
set.seed(48103)
n <- 300
sim <- data.frame(
    id = factor(sample(1:20, n, TRUE)),
    year = factor(sample(1:6, n, TRUE)),
    firm = factor(sample(1:8, n, TRUE)),
    x1 = rnorm(n),
    D = rbinom(n, 1, 0.5)
)
sim$y <- sim$D * (1 + 0.5 * sim$x1) + 0.3 * sim$x1 + as.numeric(sim$id) / 10 +
    as.numeric(sim$year) / 5 + as.numeric(sim$firm) / 7 + rnorm(n)

compare_lm <- function(mf, ml, vcov_lm = TRUE, FUN = avg_comparisons, ...) {
    a <- FUN(mf, ...)
    b <- FUN(ml, ..., vcov = vcov_lm)
    expect_equal(components(a, "jacobian_method"), "analytic")
    expect_equivalent(a$estimate, b$estimate, tolerance = 1e-10)
    expect_equivalent(a$std.error, b$std.error, tolerance = 1e-8)
}

# one, two, and three fixed effects; slopes with by
compare_lm(
    feols(y ~ D * x1 | id, data = sim, vcov = "iid"),
    lm(y ~ D * x1 + id, data = sim),
    variables = "D"
)
compare_lm(
    feols(y ~ D * x1 | id + year, data = sim, vcov = "iid"),
    lm(y ~ D * x1 + id + year, data = sim),
    FUN = avg_slopes,
    variables = "x1",
    by = "D"
)
compare_lm(
    feols(y ~ D * x1 | id + year + firm, data = sim, vcov = "iid"),
    lm(y ~ D * x1 + id + year + firm, data = sim),
    variables = "D"
)

# i() interactions
compare_lm(
    feols(y ~ D + D:x1 + i(year, x1, ref = 1) | id + year, data = sim, vcov = "iid"),
    lm(y ~ D + D:x1 + year:x1 + id + year, data = sim),
    variables = "D"
)

# heteroskedasticity-robust and clustered standard errors
ml <- lm(y ~ D * x1 + id + year, data = sim)
compare_lm(
    feols(y ~ D * x1 | id + year, data = sim, vcov = "hetero"),
    ml,
    vcov_lm = sandwich::vcovHC(ml, type = "HC1"),
    variables = "D"
)
compare_lm(
    feols(y ~ D * x1 | id + year, data = sim, vcov = ~id, ssc = ssc(K.fixef = "full")),
    ml,
    vcov_lm = sandwich::vcovCL(ml, cluster = ~id, type = "HC1"),
    variables = "D"
)
