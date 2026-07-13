source("helpers.R")
using("marginaleffects")

requiet("sandwich")

dat <- mtcars
dat$amf <- factor(dat$am)
dat$vsb <- dat$vs
dat$cylid <- dat$cyl
dat$w <- dat$wt + 0.5
dat$off <- log(dat$wt + 1)

# External reference values for unconditional linearization:
# average predictions and average comparisons for robust and clustered paths.

finite_unconditional_se <- function(x) {
    !"std.error" %in% names(x) || all(is.finite(x$std.error))
}

finite_unconditional_vcov <- function(x) {
    V <- vcov(x)
    is.matrix(V) && identical(dim(V), rep(nrow(x), 2)) && all(is.finite(V))
}

unconditional_se_matches_vcov <- function(x) {
    !"std.error" %in% names(x) ||
        isTRUE(all.equal(
            as.numeric(diag(vcov(x))),
            as.numeric(x$std.error^2),
            tolerance = 1e-7,
            check.attributes = FALSE))
}

expect_unconditional <- function(x) {
    expect_true(finite_unconditional_se(x))
    expect_true(finite_unconditional_vcov(x))
    expect_true(unconditional_se_matches_vcov(x))
}

mod_lm <- lm(mpg ~ amf + hp + wt, data = dat)

p_lm <- avg_predictions(mod_lm, variables = "amf", vcov = "unconditional", df = "residual")
expect_equivalent(p_lm$estimate, c(19.244117748653977, 21.327827876469591), tolerance = 1e-6)
expect_equivalent(p_lm$std.error, c(1.1261627856912979, 1.3306015850782165), tolerance = 1e-6)
expect_equivalent(p_lm$df, c(28, 28))
expect_unconditional(p_lm)

p_lm_robust <- avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(type = "robust"), df = "residual")
p_lm_hc1 <- avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(type = "HC1"), df = "residual")
p_lm_hc0 <- avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(type = "hc0"), df = Inf)
p_lm_hc2 <- avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(type = "HC2"))
p_lm_hc3 <- avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(type = "HC3"))
expect_equivalent(p_lm_robust$std.error, p_lm$std.error, tolerance = 1e-8)
expect_equivalent(p_lm_hc1$std.error, p_lm$std.error, tolerance = 1e-8)
expect_false("df" %in% names(p_lm_hc0))
expect_unconditional(p_lm_hc0)
expect_unconditional(p_lm_hc2)
expect_unconditional(p_lm_hc3)
expect_true(all(p_lm_hc3$std.error >= p_lm_hc2$std.error))

c_lm <- avg_comparisons(mod_lm, variables = "amf", vcov = "unconditional", df = "residual")
expect_equivalent(c_lm$estimate, 2.0837101278156140, tolerance = 1e-6)
expect_equivalent(c_lm$std.error, 1.3478880284881209, tolerance = 1e-6)
expect_unconditional(c_lm)

s_lm <- avg_slopes(mod_lm, variables = "amf", vcov = "unconditional", df = "residual")
expect_equivalent(s_lm$estimate, 2.0837101278156140, tolerance = 1e-6)
expect_equivalent(s_lm$std.error, 1.3478880284881209, tolerance = 1e-6)
expect_equivalent(s_lm$conf.low, -0.6773133, tolerance = 1e-6)
expect_unconditional(s_lm)

p_lm_cl <- avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(cluster = ~cylid), df = 2)
expect_equivalent(p_lm_cl$std.error, c(3.9317960917077559, 3.3200283031968727), tolerance = 1e-6)
expect_equivalent(p_lm_cl$df, c(2, 2))
expect_unconditional(p_lm_cl)
expect_equivalent(
    avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(cluster = ~cylid), df = 7)$df,
    c(7, 7))

s_lm_cl <- avg_slopes(mod_lm, variables = "amf", vcov = vcovUnconditional(cluster = ~cylid), df = 2)
expect_equivalent(s_lm_cl$std.error, 1.4857262433009455, tolerance = 1e-6)
expect_equivalent(s_lm_cl$df, 2)
expect_unconditional(s_lm_cl)

mod_glm <- glm(vsb ~ amf + hp + wt, family = binomial, data = dat)

p_glm <- avg_predictions(mod_glm, variables = "amf", vcov = "unconditional")
expect_equivalent(p_glm$estimate, c(0.531230955273909, 0.292614213581851), tolerance = 1e-6)
expect_equivalent(p_glm$std.error, c(0.089913330206317, 0.099846347930332), tolerance = 1e-6)
expect_false("df" %in% names(p_glm))
expect_unconditional(p_glm)

s_glm <- avg_slopes(mod_glm, variables = "amf", vcov = "unconditional")
expect_equivalent(s_glm$estimate, -0.238616741692058, tolerance = 1e-6)
expect_equivalent(s_glm$std.error, 0.098439935777850, tolerance = 1e-6)
expect_unconditional(s_glm)

p_glm_cl <- avg_predictions(mod_glm, variables = "amf", vcov = vcovUnconditional(cluster = ~cylid))
expect_equivalent(p_glm_cl$std.error, c(0.359245073856309, 0.299787890193848), tolerance = 1e-6)
expect_false("df" %in% names(p_glm_cl))
expect_unconditional(p_glm_cl)

s_glm_cl <- avg_slopes(mod_glm, variables = "amf", vcov = vcovUnconditional(cluster = ~cylid))
expect_equivalent(s_glm_cl$std.error, 0.228694962491621, tolerance = 1e-6)
expect_unconditional(s_glm_cl)

# Hansen and Overgaard (2024): equations (8), (14), and (17), and Corollary 9.
# HC0 isolates the paper's plug-in covariance from finite-sample corrections.
paper_counterfactual <- function(value, rows = seq_len(nrow(dat))) {
    newdata <- dat[rows, , drop = FALSE]
    newdata$amf <- factor(value, levels = levels(dat$amf))
    eta <- stats::predict(mod_glm, newdata = newdata, type = "link")
    X <- stats::model.matrix(
        stats::delete.response(stats::terms(mod_glm)),
        newdata,
        contrasts.arg = mod_glm$contrasts,
        xlev = mod_glm$xlevels
    )
    list(
        estimate = stats::predict(mod_glm, newdata = newdata, type = "response"),
        derivative = X * as.numeric(mod_glm$family$mu.eta(eta))
    )
}

n_paper <- nrow(dat)
beta_dot_paper <- marginaleffects:::get_unconditional_beta_dot(mod_glm)
paper_0 <- paper_counterfactual(0)
paper_1 <- paper_counterfactual(1)
theta_paper <- c(mean(paper_0$estimate), mean(paper_1$estimate))
J_paper <- rbind(
    colMeans(paper_0$derivative),
    colMeans(paper_1$derivative)
)
empirical_paper <- cbind(
    paper_0$estimate - theta_paper[[1]],
    paper_1$estimate - theta_paper[[2]]
)
Phi_paper <- empirical_paper + beta_dot_paper %*% t(J_paper)
V_paper <- crossprod(Phi_paper) / n_paper^2

means_paper <- avg_predictions(
    mod_glm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC0")
)
expect_equivalent(means_paper$estimate, theta_paper, tolerance = 1e-8)
expect_equivalent(vcov(means_paper), V_paper, tolerance = 1e-7)

ate_paper <- avg_comparisons(
    mod_glm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC0")
)
Phi_ate_paper <- Phi_paper[, 2] - Phi_paper[, 1]
expect_equivalent(ate_paper$estimate, diff(theta_paper), tolerance = 1e-8)
expect_equivalent(
    ate_paper$std.error,
    as.numeric(sqrt(crossprod(Phi_ate_paper) / n_paper^2)),
    tolerance = 1e-7
)

log_rte_paper <- avg_comparisons(
    mod_glm,
    variables = "amf",
    comparison = "lnratioavg",
    vcov = vcovUnconditional(type = "HC0")
)
Phi_log_rte_paper <-
    paper_1$estimate / theta_paper[[2]] -
    paper_0$estimate / theta_paper[[1]] +
    beta_dot_paper %*% (
        J_paper[2, ] / theta_paper[[2]] -
        J_paper[1, ] / theta_paper[[1]]
    )
expect_equivalent(log_rte_paper$estimate, log(theta_paper[[2]] / theta_paper[[1]]), tolerance = 1e-8)
expect_equivalent(
    log_rte_paper$std.error,
    as.numeric(sqrt(crossprod(Phi_log_rte_paper) / n_paper^2)),
    tolerance = 1e-7
)

treated <- which(dat$amf == "1")
paper_treated_0 <- paper_counterfactual(0, treated)
paper_treated_1 <- paper_counterfactual(1, treated)
theta_treated <- c(mean(paper_treated_0$estimate), mean(paper_treated_1$estimate))
J_treated <- rbind(
    colMeans(paper_treated_0$derivative),
    colMeans(paper_treated_1$derivative)
)
empirical_treated <- matrix(0, nrow = n_paper, ncol = 2)
empirical_treated[treated, ] <- n_paper / length(treated) * cbind(
    paper_treated_0$estimate - theta_treated[[1]],
    paper_treated_1$estimate - theta_treated[[2]]
)
Phi_treated <- empirical_treated + beta_dot_paper %*% t(J_treated)
treated_grid <- datagrid(
    model = mod_glm,
    amf = c("0", "1"),
    grid_type = "counterfactual"
)
treated_grid <- treated_grid[treated_grid$rowidcf %in% treated, ]
means_treated <- avg_predictions(
    mod_glm,
    newdata = treated_grid,
    by = "amf",
    vcov = vcovUnconditional(type = "HC0")
)
expect_equivalent(means_treated$estimate, theta_treated, tolerance = 1e-8)
expect_equivalent(vcov(means_treated), crossprod(Phi_treated) / n_paper^2, tolerance = 1e-7)

dat$cylf <- factor(dat$cyl)
mod_stress <- lm(mpg ~ amf + hp * wt + cylf, data = dat)
mod_stress_glm <- glm(vsb ~ amf + hp + wt, family = binomial, data = dat)
mod_stress_wts <- lm(mpg ~ amf + hp + wt, data = dat, weights = w)
mod_stress_offset <- lm(mpg ~ amf + hp + offset(off), data = dat)
dat_na <- dat
dat_na$hp[c(2, 7)] <- NA
mod_stress_na <- lm(mpg ~ amf + hp + wt, data = dat_na, na.action = na.omit)
bydf_partial <- data.frame(cylf = c("4", "8"), by = c("low", "high"))
cfgrid <- datagrid(model = mod_stress, hp = c(100, 150), grid_type = "counterfactual")
cfgrid_cylf <- datagrid(model = mod_stress, cylf = c("4", "6"), grid_type = "counterfactual")
cfgrid_cylf_w <- cfgrid_cylf
cfgrid_cylf_w$w <- dat$w[as.integer(cfgrid_cylf_w$rowidcf)]
custom_hyp <- function(x) data.frame(term = "custom mean", estimate = mean(x$estimate))
custom_cmp_scalar <- function(hi, lo) mean(hi - lo)

stress <- list(
    avg_predictions_base = avg_predictions(mod_stress, vcov = "unconditional"),
    avg_predictions_amf = avg_predictions(mod_stress, variables = "amf", vcov = "unconditional"),
    avg_predictions_multi = avg_predictions(mod_stress, variables = c("amf", "cylf"), vcov = "unconditional"),
    avg_predictions_by = avg_predictions(mod_stress, variables = "amf", by = "cylf", vcov = "unconditional"),
    avg_predictions_by_multi = avg_predictions(
        mod_stress,
        variables = "amf",
        by = c("amf", "cylf"),
        vcov = "unconditional"),
    avg_predictions_by_df = suppressWarnings(
        avg_predictions(mod_stress, by = bydf_partial, vcov = "unconditional")),
    avg_predictions_wts_string = avg_predictions(mod_stress, variables = "amf", wts = "w", vcov = "unconditional"),
    avg_predictions_wts_true = avg_predictions(mod_stress_wts, variables = "amf", wts = TRUE, vcov = "unconditional"),
    avg_predictions_transform = avg_predictions(
        mod_stress,
        variables = "amf",
        transform = exp,
        vcov = "unconditional"),
    avg_predictions_transform_string = avg_predictions(
        mod_stress,
        variables = "amf",
        transform = "ln",
        vcov = "unconditional"),
    avg_predictions_hypothesis = avg_predictions(
        mod_stress,
        variables = "amf",
        hypothesis = c(1, -1),
        vcov = "unconditional"),
    avg_predictions_hypothesis_function = avg_predictions(
        mod_stress,
        variables = "amf",
        hypothesis = custom_hyp,
        vcov = "unconditional"),
    avg_predictions_counterfactual = avg_predictions(
        mod_stress,
        newdata = cfgrid,
        variables = "amf",
        vcov = "unconditional"),
    avg_predictions_subset = avg_predictions(mod_stress, newdata = dat[10:20, ], vcov = "unconditional"),
    avg_predictions_offset = avg_predictions(mod_stress_offset, variables = "amf", vcov = "unconditional"),
    avg_predictions_naomit = avg_predictions(mod_stress_na, variables = "amf", vcov = "unconditional"),
    avg_predictions_cluster = avg_predictions(mod_stress, variables = "amf", vcov = vcovUnconditional(cluster = ~cylid)),
    avg_predictions_hc0 = avg_predictions(mod_stress, variables = "amf", vcov = vcovUnconditional(type = "HC0"), df = Inf),
    avg_comparisons_multi = avg_comparisons(mod_stress, variables = c("amf", "cylf"), vcov = "unconditional"),
    avg_comparisons_pairwise = avg_comparisons(
        mod_stress,
        variables = list(cylf = "pairwise"),
        vcov = "unconditional"),
    avg_comparisons_numeric = avg_comparisons(mod_stress, variables = list(hp = 10), vcov = "unconditional"),
    avg_comparisons_custom = avg_comparisons(
        mod_stress,
        variables = "amf",
        comparison = custom_cmp_scalar,
        vcov = "unconditional"),
    avg_comparisons_ratio = avg_comparisons(
        mod_stress_glm,
        variables = "amf",
        comparison = "ratio",
        vcov = "unconditional"),
    avg_comparisons_counterfactual = avg_comparisons(
        mod_stress,
        newdata = cfgrid_cylf,
        variables = list(hp = 10),
        by = "cylf",
        vcov = "unconditional"),
    avg_comparisons_counterfactual_wts = avg_comparisons(
        mod_stress,
        newdata = cfgrid_cylf_w,
        variables = list(hp = 10),
        by = "cylf",
        wts = "w",
        vcov = "unconditional"),
    avg_slopes_numeric = avg_slopes(mod_stress, variables = "hp", vcov = "unconditional"),
    avg_slopes_by = avg_slopes(mod_stress, variables = "hp", by = "cylf", vcov = "unconditional"),
    avg_slopes_wts = avg_slopes(mod_stress_wts, variables = "hp", wts = TRUE, vcov = "unconditional"),
    avg_slopes_eyex = avg_slopes(mod_stress_glm, variables = "hp", slope = "eyex", vcov = "unconditional"),
    avg_slopes_counterfactual = avg_slopes(
        mod_stress,
        newdata = cfgrid_cylf,
        variables = "hp",
        by = "cylf",
        vcov = "unconditional")
)

expect_true(all(vapply(stress, finite_unconditional_se, logical(1))))
expect_true(all(vapply(stress, finite_unconditional_vcov, logical(1))))
expect_true(all(vapply(stress, unconditional_se_matches_vcov, logical(1))))
expect_true(nrow(stress$avg_predictions_multi) > 2)
expect_true(nrow(stress$avg_comparisons_pairwise) == 3)
expect_true(all(is.finite(hypotheses(p_lm, hypothesis = "b1 = b2")$std.error)))
expect_true(all(is.finite(hypotheses(p_lm, joint = TRUE)$statistic)))
expect_true(all(is.finite(hypotheses(stress$avg_comparisons_pairwise, hypothesis = ~pairwise)$std.error)))

set.seed(42)
n_wide <- 180
wide <- data.frame(
    trt = factor(rbinom(n_wide, 1, 0.45)),
    cluster = sample(1:18, n_wide, replace = TRUE)
)
for (j in 1:20) {
    wide[[paste0("x", j)]] <- rnorm(n_wide)
}
wide$y <- 1 +
    0.8 * as.numeric(wide$trt == "1") +
    Reduce(`+`, Map(function(j) j / 50 * wide[[paste0("x", j)]], 1:20)) +
    rnorm(n_wide)
wide_formula <- stats::reformulate(c("trt", paste0("x", 1:20)), response = "y")
mod_wide <- lm(wide_formula, data = wide)
wide_results <- list(
    predictions = avg_predictions(mod_wide, variables = "trt", vcov = "unconditional"),
    comparisons = avg_comparisons(mod_wide, variables = "trt", vcov = vcovUnconditional(cluster = ~cluster)),
    slopes = avg_slopes(mod_wide, variables = paste0("x", 1:5), vcov = "unconditional")
)
expect_true(all(vapply(wide_results, finite_unconditional_se, logical(1))))
expect_true(all(vapply(wide_results, finite_unconditional_vcov, logical(1))))
expect_true(all(vapply(wide_results, unconditional_se_matches_vcov, logical(1))))

expect_error(
    predictions(mod_lm, vcov = "unconditional"),
    pattern = "averaged or aggregated"
)
expect_error(
    avg_predictions(mod_lm, newdata = "mean", vcov = "unconditional"),
    pattern = "row IDs"
)
expect_error(
    avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(cluster = ~cylid + amf)),
    pattern = "one-way"
)
expect_error(
    avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(cluster = cylid ~ 1)),
    pattern = "one-sided"
)
expect_error(
    avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(cluster = ~missing_cluster)),
    pattern = "not found"
)
dat$c_bad <- dat$cylid
dat$c_bad[1] <- NA
mod_cluster_na <- lm(mpg ~ amf + hp + wt, data = dat)
expect_error(
    avg_predictions(mod_cluster_na, variables = "amf", vcov = vcovUnconditional(cluster = ~c_bad)),
    pattern = "missing"
)
dat$c_one <- 1
mod_one_cluster <- lm(mpg ~ amf + hp + wt, data = dat)
expect_error(
    avg_predictions(mod_one_cluster, variables = "amf", vcov = vcovUnconditional(cluster = ~c_one)),
    pattern = "at least two clusters"
)
expect_error(
    avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(type = "HC9")),
    pattern = "`type` must be one of"
)
expect_error(
    avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(cluster = "cylid")),
    pattern = "one-sided formula"
)
for (type_cluster in c("HC2", "HC3", "HC4", "HC4m", "HC5")) {
    expect_error(
        avg_predictions(
            mod_lm,
            variables = "amf",
            vcov = vcovUnconditional(type = type_cluster, cluster = ~cylid)
        ),
        pattern = "does not currently support.*with `cluster`"
    )
}
expect_unconditional(
    avg_predictions(
        mod_lm,
        variables = "amf",
        vcov = vcovUnconditional(type = "HC0", cluster = ~cylid)
    )
)
expect_unconditional(
    avg_predictions(
        mod_lm,
        variables = "amf",
        vcov = vcovUnconditional(type = "HC1", cluster = ~cylid)
    )
)
expect_error(
    avg_predictions(mod_lm, variables = "amf", vcov = vcovUnconditional(), df = c(1, 2, 3)),
    pattern = "length 1"
)
expect_inherits(
    marginaleffects:::sanitize_vcov_request("UnConDiTiOnAl"),
    "marginaleffects_vcov_unconditional"
)
expect_identical(marginaleffects:::sanitize_vcov_request("HC1"), "HC1")
dat_saturated <- data.frame(y = c(1, 2, 3, 4), x = factor(1:4))
mod_saturated <- lm(y ~ x, data = dat_saturated)
expect_error(
    avg_predictions(mod_saturated, vcov = "unconditional", df = "residual"),
    pattern = "more observations than estimated coefficients"
)
expect_unconditional(avg_predictions(mod_saturated, vcov = vcovUnconditional(type = "HC0"), df = Inf))
expect_error(
    hypotheses(mod_lm, vcov = "unconditional"),
    pattern = "avg_predictions"
)
expect_error(
    hypotheses(p_lm, joint = TRUE, vcov = "unconditional"),
    pattern = "initial `marginaleffects` call"
)
expect_error(
    inferences(p_lm, method = "boot", R = 2),
    pattern = "not available"
)
mod_mlm <- lm(cbind(mpg, disp) ~ hp + wt, data = dat)
expect_error(
    avg_predictions(mod_mlm, vcov = "unconditional"),
    pattern = "not currently supported"
)

phi_numderiv <- marginaleffects:::apply_unconditional_hypothesis_phi(
    phi = matrix(1, nrow = 1, ncol = 1),
    plan = list(agg = NULL, hyp = list(apply = function(x) x^2)),
    pre_hypothesis_estimate = 2,
    numderiv = list("fdcenter", eps = 0.2))
expect_equivalent(phi_numderiv, matrix(4, nrow = 1, ncol = 1), tolerance = 1e-10)
expect_error(
    marginaleffects:::resolve_unconditional_rowid(
        data.frame(rowidcf = 1:64),
        n = nrow(dat)),
    pattern = "map to the original model data")

if (requiet("MASS")) {
    dat_polr <- dat
    dat_polr$mpg3 <- ordered(cut(dat_polr$mpg, breaks = 3, labels = c("low", "mid", "high")))
    mod_polr <- MASS::polr(mpg3 ~ hp + wt, data = dat_polr, Hess = TRUE)
    expect_inherits(avg_predictions(mod_polr, vcov = FALSE), "predictions")
    expect_error(
        avg_predictions(mod_polr, vcov = "unconditional"),
        pattern = "not currently supported"
    )
}

if (requiet("survey")) {
    data("api", package = "survey")
    design_svy <- survey::svydesign(
        id = ~dnum,
        weights = ~pw,
        data = apiclus1,
        fpc = ~fpc)
    mod_svy <- survey::svyglm(api00 ~ ell + meals + sch.wide, design = design_svy)
    expect_error(
        suppressWarnings(avg_predictions(mod_svy, vcov = "unconditional")),
        pattern = "not currently supported"
    )
}

if (requiet("mice")) {
    set.seed(2048)
    dat_mi <- dat
    dat_mi$hp[c(1, 4, 7)] <- NA
    imp <- mice::mice(dat_mi, m = 2, maxit = 1, printFlag = FALSE)
    mod_mi <- with(imp, lm(mpg ~ hp + wt + am))
    expect_error(
        avg_predictions(mod_mi, vcov = "unconditional"),
        pattern = "multiple-imputation"
    )
    expect_error(
        hypotheses(mod_mi, hypothesis = "hp = 0", vcov = vcovUnconditional()),
        pattern = "multiple-imputation"
    )
}

if (requiet("AER")) {
    mod_tobit <- AER::tobit(mpg ~ amf + hp + wt, left = 10, data = dat)
    tobit_results <- list(
        predictions = avg_predictions(mod_tobit, variables = "amf", vcov = "unconditional"),
        comparisons = avg_comparisons(mod_tobit, variables = "amf", vcov = "unconditional"),
        slopes = avg_slopes(mod_tobit, variables = "hp", vcov = "unconditional")
    )
    expect_true(all(vapply(tobit_results, finite_unconditional_se, logical(1))))
    expect_true(all(vapply(tobit_results, finite_unconditional_vcov, logical(1))))
    expect_true(all(vapply(tobit_results, unconditional_se_matches_vcov, logical(1))))
}

if (requiet("survival")) {
    lung <- survival::lung
    lung$status2 <- as.integer(lung$status == 2)
    mod_cox <- survival::coxph(survival::Surv(time, status2) ~ age + sex, data = lung)
    cox_risk <- avg_slopes(mod_cox, variables = "age", type = "risk", vcov = "unconditional")
    expect_unconditional(cox_risk)
    expect_error(
        avg_predictions(mod_cox, type = "survival", vcov = "unconditional"),
        pattern = "baseline hazard"
    )
}

if (requiet("fixest")) {
    mod_fixest <- fixest::feols(mpg ~ amf + hp + wt, data = dat)
    s_fixest <- avg_slopes(mod_fixest, variables = "amf", vcov = "unconditional")
    expect_equivalent(s_fixest$estimate, s_lm$estimate, tolerance = 1e-6)
    expect_equivalent(s_fixest$std.error, s_lm$std.error, tolerance = 1e-6)

    mod_fixest_fe <- fixest::feols(mpg ~ amf + hp + wt | cylid, data = dat)
    expect_error(
        avg_predictions(mod_fixest_fe, vcov = "unconditional"),
        pattern = "fixed effects"
    )
    fixest_fe_inputs <- list(
        model = mod_fixest_fe,
        n = mod_fixest_fe$nobs,
        vcov = list(type = "HC1", cluster = NULL, cluster_var = NULL))
    expect_equivalent(
        marginaleffects:::get_unconditional_correction(fixest_fe_inputs),
        mod_fixest_fe$nobs / stats::df.residual(mod_fixest_fe))
    fixest_fe_cluster <- fixest_fe_inputs
    fixest_fe_cluster$vcov <- list(type = "HC1", cluster = dat$cylid, cluster_var = "cylid")
    expect_equivalent(
        marginaleffects:::get_unconditional_correction(fixest_fe_cluster),
        (length(unique(dat$cylid)) / (length(unique(dat$cylid)) - 1)) *
            ((mod_fixest_fe$nobs - 1) / stats::df.residual(mod_fixest_fe)))
    s_fixest_fe <- avg_slopes(mod_fixest_fe, variables = "amf", vcov = vcovUnconditional(cluster = ~cylid))
    expect_unconditional(s_fixest_fe)

    # Absorbed fixed effects cancel from additive differences and from slopes
    # which only rescale hi - lo, provided the fixed-effect variables do not
    # change between the counterfactual predictions.
    expect_unconditional(
        avg_comparisons(mod_fixest_fe, variables = "amf", vcov = "unconditional")
    )
    expect_unconditional(
        avg_comparisons(
            mod_fixest_fe,
            variables = "amf",
            wts = "w",
            vcov = "unconditional"
        )
    )
    expect_unconditional(
        avg_slopes(
            mod_fixest_fe,
            variables = "hp",
            slope = "dyex",
            vcov = "unconditional"
        )
    )

    expect_error(
        avg_comparisons(
            mod_fixest_fe,
            variables = "amf",
            comparison = "ratio",
            vcov = "unconditional"
        ),
        pattern = "only for additive differences"
    )
    expect_error(
        avg_slopes(
            mod_fixest_fe,
            variables = "hp",
            slope = "eyex",
            vcov = "unconditional"
        ),
        pattern = "only for additive differences"
    )
    expect_error(
        avg_comparisons(
            mod_fixest_fe,
            variables = "amf",
            comparison = function(hi, lo) mean(hi - lo),
            vcov = "unconditional"
        ),
        pattern = "custom function"
    )
    expect_error(
        avg_comparisons(mod_fixest_fe, variables = "cylid", vcov = "unconditional"),
        pattern = "cannot vary fixed-effect or varying-slope variables.*`cylid`"
    )

    mod_fixest_vs <- fixest::feols(mpg ~ amf + wt | cylid[hp], data = dat)
    expect_unconditional(
        avg_comparisons(mod_fixest_vs, variables = "amf", vcov = "unconditional")
    )
    expect_error(
        avg_slopes(mod_fixest_vs, variables = "hp", vcov = "unconditional"),
        pattern = "cannot vary fixed-effect or varying-slope variables.*`hp`"
    )

    # `fixef_vars` stores this as the single string "cylid^gear". The
    # validator must parse the fixed-effect formula and detect `gear` itself.
    mod_fixest_combined <- fixest::feols(mpg ~ amf + hp + wt | cylid^gear, data = dat)
    combined_hi <- dat
    combined_lo <- dat
    combined_hi$gear <- combined_hi$gear + 1
    combined_plan <- list(
        kind = "comparisons",
        groups = list(list(fun_key = "difference")),
        predict_args = list(hi = combined_hi, lo = combined_lo)
    )
    expect_error(
        marginaleffects:::validate_unconditional_fixest_plan(
            mod_fixest_combined,
            combined_plan
        ),
        pattern = "cannot vary fixed-effect or varying-slope variables.*`gear`"
    )

    mod_fixest_nonlinear <- fixest::fepois(am ~ hp + wt | cylid, data = dat)
    expect_error(
        avg_slopes(mod_fixest_nonlinear, variables = "hp", vcov = "unconditional"),
        pattern = "nonlinear.*fixed effects"
    )
}

if (requiet("etwfe")) {
    set.seed(1024)
    n_id <- 40
    n_time <- 6
    dat_etwfe <- expand.grid(id = seq_len(n_id), tt = seq_len(n_time))
    cohort <- sample(c(0, 3, 4, 5), n_id, replace = TRUE)
    dat_etwfe$g <- cohort[dat_etwfe$id]
    dat_etwfe$treat <- dat_etwfe$g > 0 & dat_etwfe$tt >= dat_etwfe$g
    dat_etwfe$x <- rnorm(n_id)[dat_etwfe$id]
    dat_etwfe$y <- rnorm(n_id)[dat_etwfe$id] +
        rnorm(n_time)[dat_etwfe$tt] +
        0.5 * dat_etwfe$x +
        1.2 * dat_etwfe$treat +
        rnorm(nrow(dat_etwfe))

    mod_etwfe <- etwfe::etwfe(
        y ~ x,
        tvar = tt,
        gvar = g,
        ivar = id,
        data = dat_etwfe)
    emfx_robust <- etwfe::emfx(mod_etwfe, vcov = "unconditional")
    emfx_cluster <- etwfe::emfx(mod_etwfe, vcov = vcovUnconditional(cluster = ~id))
    expect_unconditional(emfx_robust)
    expect_unconditional(emfx_cluster)
}

rm(list = ls())
