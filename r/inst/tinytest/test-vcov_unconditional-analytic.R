source("helpers.R")
using("marginaleffects")

# Validate the analytic empirical influence functions against numerical
# infinitesimal-contamination derivatives of a separate code path. The oracle
# calls the existing weighted comparison functions as black boxes; this test
# does not reproduce the analytic formulas from vcov_unconditional.R.
contamination_if <- function(fun, hi, lo, observation_weights, eps = 1e-6) {
    m <- length(hi)
    vapply(seq_len(m), function(i) {
        evaluate <- function(e) {
            mass <- rep((1 - e) / m, m)
            mass[[i]] <- mass[[i]] + e
            fun(hi, lo, w = mass * observation_weights)
        }
        (evaluate(eps) - evaluate(-eps)) / (2 * eps)
    }, numeric(1))
}

analytic_if <- function(key, hi, lo, w = NULL, n = length(hi)) {
    group <- list(
        fun_key = key,
        idx = seq_along(hi),
        args = if (is.null(w)) list() else list(w = w)
    )
    marginaleffects:::get_unconditional_known_scalar_comparison_phi(
        group = group,
        hi = hi,
        lo = lo,
        rowid = seq_along(hi),
        n = n
    )
}

# Positive values strictly inside (0, 1) keep every contrast, including the
# log ratio and log odds ratio, on its smooth interior domain.
hi <- c(0.31, 0.47, 0.58, 0.69, 0.76, 0.84)
lo <- c(0.18, 0.29, 0.36, 0.44, 0.53, 0.61)
w <- c(0.5, 1.2, 2.7, 0.8, 3.1, 1.6)
m <- length(hi)

keys <- c("differenceavg", "ratioavg", "lnratioavg", "lnoravg", "liftavg")
phi_unweighted <- list()
phi_weighted <- list()

for (key in keys) {
    weighted_key <- paste0(key, "wts")
    oracle <- marginaleffects:::comparison_function_dict[[weighted_key]]

    # With unit observation weights, contamination of the weighted target is
    # the empirical influence function of the corresponding unweighted target.
    expected <- contamination_if(
        fun = oracle,
        hi = hi,
        lo = lo,
        observation_weights = rep(1, m)
    )
    phi_unweighted[[key]] <- analytic_if(key, hi, lo)
    expect_equivalent(phi_unweighted[[key]], expected, tolerance = 1e-6)
    expect_equivalent(sum(phi_unweighted[[key]]), 0, tolerance = 1e-10)

    # Unequal observation weights validate both the weighted means and the
    # n * w_i / sum(w) contribution scaling.
    expected_weighted <- contamination_if(
        fun = oracle,
        hi = hi,
        lo = lo,
        observation_weights = w
    )
    phi_weighted[[key]] <- analytic_if(weighted_key, hi, lo, w = w)
    expect_equivalent(phi_weighted[[key]], expected_weighted, tolerance = 1e-6)
    expect_equivalent(sum(phi_weighted[[key]]), 0, tolerance = 1e-10)

    # A group containing m of n observations is represented on the full-sample
    # influence scale by n / m; rows outside the group contribute zero.
    subgroup <- analytic_if(weighted_key, hi, lo, w = w, n = 2 * m)
    expect_equivalent(subgroup[seq_len(m)], 2 * expected_weighted, tolerance = 1e-6)
    expect_equivalent(subgroup[m + seq_len(m)], rep(0, m), tolerance = 1e-10)
}

# Lift differs from a ratio only by the constant one, so their influence
# functions must be identical for both empirical targets.
expect_equivalent(phi_unweighted[["liftavg"]], phi_unweighted[["ratioavg"]], tolerance = 1e-10)
expect_equivalent(phi_weighted[["liftavg"]], phi_weighted[["ratioavg"]], tolerance = 1e-10)


# Independent end-to-end HC0 references -----------------------------------

# These references construct IF(beta) directly from model matrices and score
# equations. They deliberately avoid sandwich::estfun(), sandwich::bread(),
# and get_unconditional_beta_dot(), except when comparing that helper to the
# independently constructed result.
set.seed(481)
n_model <- 120
dat <- data.frame(
    x = seq(-1.5, 1.5, length.out = n_model),
    trt = factor(rep(0:1, length.out = n_model))
)
dat$y_continuous <-
    1 + 0.7 * (dat$trt == "1") + 0.8 * dat$x + cos(seq_len(n_model) / 3)
probability <- plogis(-0.3 + 0.7 * (dat$trt == "1") + 0.8 * dat$x)
dat$y_binary <- rbinom(n_model, size = 1, prob = probability)

counterfactual_matrix <- function(model, data, value) {
    newdata <- data
    newdata$trt <- factor(value, levels = levels(data$trt))
    stats::model.matrix(
        stats::delete.response(stats::terms(model)),
        newdata,
        contrasts.arg = model$contrasts,
        xlev = model$xlevels
    )
}

mod_lm <- lm(y_continuous ~ trt + x, data = dat)
X_lm <- stats::model.matrix(mod_lm)
beta_if_lm <-
    sweep(X_lm, 1, stats::residuals(mod_lm), `*`) %*%
    solve(crossprod(X_lm) / n_model)
expect_equivalent(
    marginaleffects:::get_unconditional_beta_dot(mod_lm),
    beta_if_lm,
    tolerance = 1e-9
)

X_lm_0 <- counterfactual_matrix(mod_lm, dat, 0)
X_lm_1 <- counterfactual_matrix(mod_lm, dat, 1)
prediction_lm_0 <- drop(X_lm_0 %*% stats::coef(mod_lm))
prediction_lm_1 <- drop(X_lm_1 %*% stats::coef(mod_lm))
jacobian_lm <- rbind(colMeans(X_lm_0), colMeans(X_lm_1))
empirical_lm <- cbind(
    prediction_lm_0 - mean(prediction_lm_0),
    prediction_lm_1 - mean(prediction_lm_1)
)
Phi_lm <- empirical_lm + beta_if_lm %*% t(jacobian_lm)
V_lm <- crossprod(Phi_lm) / n_model^2

out_lm <- avg_predictions(
    mod_lm,
    variables = "trt",
    vcov = vcovUnconditional(type = "HC0")
)
expect_equivalent(
    out_lm$estimate,
    c(mean(prediction_lm_0), mean(prediction_lm_1)),
    tolerance = 1e-9
)
# The public path obtains the final target Jacobian numerically, whereas this
# reference uses its exact design-matrix derivative.
expect_equivalent(vcov(out_lm), V_lm, tolerance = 1e-6)

comparison_lm <- avg_comparisons(
    mod_lm,
    variables = "trt",
    comparison = "differenceavg",
    vcov = vcovUnconditional(type = "HC0")
)
Phi_lm_difference <- Phi_lm[, 2] - Phi_lm[, 1]
expect_equivalent(
    vcov(comparison_lm),
    crossprod(Phi_lm_difference) / n_model^2,
    tolerance = 1e-6
)

# For a canonical logit GLM, the score is X_i * (y_i - mu_i) and the
# sensitivity is X' diag(mu * (1 - mu)) X / n. Tight GLM convergence still
# leaves tiny differences between the final fitted means and the stored IRLS
# working weights, so the direct helper comparison uses a modest tolerance.
mod_glm <- glm(
    y_binary ~ trt + x,
    family = binomial(),
    data = dat,
    control = glm.control(epsilon = 1e-12, maxit = 100)
)
X_glm <- stats::model.matrix(mod_glm)
y_glm <- stats::model.response(stats::model.frame(mod_glm))
mu_glm <- stats::fitted(mod_glm)
working_variance <- mu_glm * (1 - mu_glm)
score_glm <- sweep(X_glm, 1, y_glm - mu_glm, `*`)
sensitivity_glm <-
    crossprod(X_glm, sweep(X_glm, 1, working_variance, `*`)) / n_model
beta_if_glm <- score_glm %*% solve(sensitivity_glm)
expect_equivalent(
    marginaleffects:::get_unconditional_beta_dot(mod_glm),
    beta_if_glm,
    tolerance = 1e-4
)

X_glm_0 <- counterfactual_matrix(mod_glm, dat, 0)
X_glm_1 <- counterfactual_matrix(mod_glm, dat, 1)
prediction_glm_0 <- plogis(drop(X_glm_0 %*% stats::coef(mod_glm)))
prediction_glm_1 <- plogis(drop(X_glm_1 %*% stats::coef(mod_glm)))
jacobian_glm <- rbind(
    colMeans(X_glm_0 * prediction_glm_0 * (1 - prediction_glm_0)),
    colMeans(X_glm_1 * prediction_glm_1 * (1 - prediction_glm_1))
)
empirical_glm <- cbind(
    prediction_glm_0 - mean(prediction_glm_0),
    prediction_glm_1 - mean(prediction_glm_1)
)
Phi_glm <- empirical_glm + beta_if_glm %*% t(jacobian_glm)
V_glm <- crossprod(Phi_glm) / n_model^2

out_glm <- avg_predictions(
    mod_glm,
    variables = "trt",
    vcov = vcovUnconditional(type = "HC0")
)
expect_equivalent(
    out_glm$estimate,
    c(mean(prediction_glm_0), mean(prediction_glm_1)),
    tolerance = 1e-9
)
expect_equivalent(vcov(out_glm), V_glm, tolerance = 1e-6)

comparison_glm <- avg_comparisons(
    mod_glm,
    variables = "trt",
    comparison = "differenceavg",
    vcov = vcovUnconditional(type = "HC0")
)
Phi_glm_difference <- Phi_glm[, 2] - Phi_glm[, 1]
expect_equivalent(
    vcov(comparison_glm),
    crossprod(Phi_glm_difference) / n_model^2,
    tolerance = 1e-6
)
