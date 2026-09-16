source("helpers.R")
using("marginaleffects")

if (!requiet("WeightIt") || !requiet("sandwich") || !requiet("cobalt")) {
    exit_file("WeightIt")
}

data("lalonde", package = "cobalt")
dat <- lalonde
dat$re78pos <- as.numeric(dat$re78 > 0)

f_out <- re78 ~ treat * (age + educ + re74)
f_bin <- re78pos ~ treat * (age + educ + re74)


# Without a `weightit` object, `glm_weightit()` reduces to `glm()`, so the
# unconditional standard errors must agree. This is the sharpest available check
# that the score/bread contract is respected. The Gaussian case agrees to
# machine precision; the logit case is limited by `WeightIt` solving its
# estimating equations numerically rather than by IRLS.
for (spec in list(
    list(formula = f_out, family = gaussian(), tolerance = 1e-10),
    list(formula = f_bin, family = binomial(), tolerance = 1e-5)
)) {
    mod_w <- WeightIt::glm_weightit(spec$formula, data = dat, family = spec$family)
    mod_g <- glm(spec$formula, data = dat, family = spec$family)

    cmp_w <- avg_comparisons(mod_w, variables = "treat", vcov = vcovUnconditional())
    cmp_g <- avg_comparisons(mod_g, variables = "treat", vcov = vcovUnconditional())
    expect_equivalent(cmp_w$estimate, cmp_g$estimate, tolerance = spec$tolerance)
    expect_equivalent(cmp_w$std.error, cmp_g$std.error, tolerance = spec$tolerance)

    prd_w <- avg_predictions(mod_w, by = "treat", vcov = vcovUnconditional())
    prd_g <- avg_predictions(mod_g, by = "treat", vcov = vcovUnconditional())
    expect_equivalent(prd_w$estimate, prd_g$estimate, tolerance = spec$tolerance)
    expect_equivalent(prd_w$std.error, prd_g$std.error, tolerance = spec$tolerance)
}

# `lm_weightit()` returns an object of class `glm_weightit`, so it takes the
# same path.
mod_lmw <- WeightIt::lm_weightit(f_out, data = dat)
expect_inherits(mod_lmw, "glm_weightit")
expect_true(all(is.finite(
    avg_comparisons(mod_lmw, variables = "treat", vcov = vcovUnconditional())$std.error)))


# With estimated propensity-score weights, `WeightIt` folds the
# weight-estimation correction into `estfun()`, so `scores %*% bread` remains a
# complete influence function. Verify that contract directly, then check that
# the unconditional path runs.
w <- WeightIt::weightit(
    treat ~ age + educ + race + married + nodegree + re74 + re75,
    data = dat,
    method = "glm",
    estimand = "ATE")
mod <- WeightIt::glm_weightit(f_out, data = dat, weightit = w)

scores <- sandwich::estfun(mod)
bread <- sandwich::bread(mod)
V <- crossprod(scores %*% bread) / nrow(scores)^2
expect_equivalent(V, vcov(mod), tolerance = 1e-8)

cmp <- avg_comparisons(mod, variables = "treat", vcov = vcovUnconditional())
expect_true(all(is.finite(cmp$std.error)))
expect_equivalent(as.numeric(diag(vcov(cmp))), cmp$std.error^2, tolerance = 1e-8)

cmp_hc1 <- avg_comparisons(mod, variables = "treat", vcov = vcovUnconditional(type = "HC1"))
expect_true(cmp_hc1$std.error > cmp$std.error)

dat$clust <- rep_len(seq_len(20), nrow(dat))
mod_clust <- WeightIt::glm_weightit(f_out, data = dat, weightit = w)
cmp_clust <- avg_comparisons(
    mod_clust,
    variables = "treat",
    vcov = vcovUnconditional(cluster = ~clust))
expect_true(all(is.finite(cmp_clust$std.error)))


# The multi-equation and survival siblings are not supported: grouped estimate
# tables drop the empirical-distribution term for every group after the first,
# and `coxph_weightit` hits the censored-model guard.
dat$re78cat <- cut(dat$re78, breaks = c(-1, 0, 5000, Inf), labels = c("a", "b", "c"))
mod_multinom <- WeightIt::multinom_weightit(re78cat ~ treat + age, data = dat)
expect_error(
    avg_comparisons(mod_multinom, variables = "treat", vcov = vcovUnconditional()),
    pattern = "not currently supported")

dat$re78ord <- factor(dat$re78cat, ordered = TRUE)
mod_ordinal <- WeightIt::ordinal_weightit(re78ord ~ treat + age, data = dat)
expect_error(
    avg_comparisons(mod_ordinal, variables = "treat", vcov = vcovUnconditional()),
    pattern = "not currently supported")

if (requiet("survival")) {
    dat$time <- dat$re78 + 1
    dat$event <- dat$re78pos
    mod_cox <- WeightIt::coxph_weightit(
        survival::Surv(time, event) ~ treat + age,
        data = dat)
    expect_error(
        avg_comparisons(mod_cox, variables = "treat", vcov = vcovUnconditional()),
        pattern = "censored or survival")
}
