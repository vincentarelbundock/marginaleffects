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


# `multinom_weightit()` and `ordinal_weightit()` return one block of predictions
# per outcome level, and every level needs its own empirical-distribution term.
dat$re78cat <- cut(dat$re78, breaks = c(-1, 0, 5000, Inf), labels = c("a", "b", "c"))
dat$re78ord <- factor(dat$re78cat, ordered = TRUE)
f_cat <- re78cat ~ treat * (age + educ)
f_ord <- re78ord ~ treat * (age + educ)

# Without a `weightit` object, they reduce to `nnet::multinom()` and
# `MASS::polr()`. `polr()` stops its optimizer earlier than `WeightIt`.
if (requiet("nnet") && requiet("MASS")) {
    for (spec in list(
        list(
            w = WeightIt::multinom_weightit(f_cat, data = dat),
            m = nnet::multinom(f_cat, data = dat, trace = FALSE, reltol = 1e-12, maxit = 1000),
            tolerance = 1e-6),
        list(
            w = WeightIt::ordinal_weightit(f_ord, data = dat),
            m = MASS::polr(f_ord, data = dat, Hess = TRUE),
            tolerance = 1e-3)
    )) {
        cmp_w <- avg_comparisons(spec$w, variables = "treat", vcov = vcovUnconditional())
        cmp_m <- avg_comparisons(spec$m, variables = "treat", vcov = vcovUnconditional())
        expect_equivalent(cmp_w$estimate, cmp_m$estimate, tolerance = spec$tolerance)
        expect_equivalent(cmp_w$std.error, cmp_m$std.error, tolerance = spec$tolerance)
    }
}

# With estimated weights, compare every outcome level with an influence
# function built from `estfun()`, `bread()`, and `predict()`.
manual_avg_probs <- function(mod, data) {
    b <- coef(mod)
    if_beta <- (sandwich::estfun(mod) %*% sandwich::bread(mod))[, names(b)]
    probs <- function(beta) {
        mod$coefficients[] <- beta
        predict(mod, newdata = data, type = "probs")
    }
    p <- probs(b)
    J <- numDeriv::jacobian(function(beta) colMeans(probs(beta)), b)
    phi <- sweep(p, 2, colMeans(p)) + if_beta %*% t(J)
    list(estimate = colMeans(p), std.error = sqrt(diag(crossprod(phi))) / nrow(p))
}

if (requiet("numDeriv")) {
    for (mod in list(
        WeightIt::multinom_weightit(f_cat, data = dat, weightit = w),
        WeightIt::ordinal_weightit(f_ord, data = dat, weightit = w)
    )) {
        ref <- manual_avg_probs(mod, dat)
        prd <- avg_predictions(mod, vcov = vcovUnconditional())
        expect_equivalent(prd$estimate, ref$estimate, tolerance = 1e-8)
        expect_equivalent(prd$std.error, ref$std.error, tolerance = 1e-6)
    }
}

# With two outcome levels, both reduce to `glm_weightit()` with a logit link.
dat$re78fac <- factor(dat$re78pos)
dat$re78bin <- factor(dat$re78pos, ordered = TRUE)
f_rhs <- ~ treat * (age + educ)
mod_glm <- WeightIt::glm_weightit(update(f_rhs, re78pos ~ .), data = dat, weightit = w,
    family = binomial())
cmp_glm <- avg_comparisons(mod_glm, variables = "treat", vcov = vcovUnconditional())
mods_two <- list(
    WeightIt::multinom_weightit(update(f_rhs, re78fac ~ .), data = dat, weightit = w))
# `ordinal_weightit()` cannot fit a two-level outcome before WeightIt 2.2.0: the
# optimizer stops early and the Hessian cannot be inverted.
if (utils::packageVersion("WeightIt") >= "2.2.0") {
    mods_two[[2]] <- WeightIt::ordinal_weightit(update(f_rhs, re78bin ~ .), data = dat,
        weightit = w)
}
for (mod in mods_two) {
    cmp <- avg_comparisons(mod, variables = "treat", vcov = vcovUnconditional())
    cmp <- cmp[cmp$group == "1", ]
    expect_equivalent(cmp$estimate, cmp_glm$estimate, tolerance = 1e-5)
    expect_equivalent(cmp$std.error, cmp_glm$std.error, tolerance = 1e-5)
}


# `newdata = subset(treat == 1)` targets the ATT. Its `rowid` restarts at 1, so
# its rows are mapped back to the model data, after which the result must
# match the treated subgroup of `by = "treat"`. Shuffle the data so the treated
# units are not the first rows, where the restarted `rowid` lines up by chance.
set.seed(1)
dat_att <- dat[sample(nrow(dat)), ]
rownames(dat_att) <- NULL
w_att <- WeightIt::weightit(
    treat ~ age + educ + race + married + nodegree + re74 + re75,
    data = dat_att,
    method = "glm",
    estimand = "ATT")
for (mod in list(
    WeightIt::glm_weightit(f_bin, data = dat_att, weightit = w_att, family = binomial()),
    WeightIt::multinom_weightit(f_cat, data = dat_att, weightit = w_att),
    WeightIt::ordinal_weightit(f_ord, data = dat_att, weightit = w_att)
)) {
    ref <- avg_comparisons(mod, variables = "treat", by = "treat", vcov = vcovUnconditional())
    ref <- ref[ref$treat == 1, ]

    att <- avg_comparisons(mod, variables = "treat", newdata = subset(treat == 1),
        vcov = vcovUnconditional())
    expect_equivalent(att$estimate, ref$estimate, tolerance = 1e-10)
    expect_equivalent(att$std.error, ref$std.error, tolerance = 1e-10)

    # A data frame subset by the user takes the same path.
    att_df <- avg_comparisons(mod, variables = "treat",
        newdata = subset(dat_att, treat == 1), vcov = vcovUnconditional())
    expect_equivalent(att_df$std.error, ref$std.error, tolerance = 1e-10)

    # Potential-outcome means among the treated come from a counterfactual grid
    # built on the subset. Their difference is the ATT.
    hyp <- if (inherits(mod, "glm_weightit")) {
        difference ~ reference
    } else {
        difference ~ reference | group
    }
    po <- avg_predictions(mod, variables = list(treat = 0:1),
        newdata = subset(treat == 1), hypothesis = hyp, vcov = vcovUnconditional())
    expect_equivalent(po$estimate, ref$estimate, tolerance = 1e-8)
    expect_equivalent(po$std.error, ref$std.error, tolerance = 1e-6)
}


# `coxph_weightit` hits the censored-model guard.
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
