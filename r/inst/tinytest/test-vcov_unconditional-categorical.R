source("helpers.R")
using("marginaleffects")

if (!requiet("nnet") || !requiet("MASS") || !requiet("sandwich") ||
    !requiet("numDeriv") || !requiet("cobalt")) {
    exit_file("nnet, MASS, sandwich, numDeriv, or cobalt not installed")
}

# Multinomial and ordinal models return one block of predictions per outcome
# level. Each level's unconditional standard error must include its own
# empirical-distribution term. The references below compute the probabilities
# directly from the model matrix, so they share no code with the prediction
# methods.

data("lalonde", package = "cobalt")
set.seed(1)
dat <- lalonde[sample(nrow(lalonde)), ]
rownames(dat) <- NULL
dat$re78cat <- cut(dat$re78, breaks = c(-1, 0, 5000, Inf), labels = c("a", "b", "c"))
dat$re78ord <- factor(dat$re78cat, ordered = TRUE)

rhs <- ~ treat * (age + educ) + race
X <- model.matrix(rhs, dat)
n <- nrow(dat)

mod_multinom <- nnet::multinom(update(rhs, re78cat ~ .), data = dat,
    trace = FALSE, reltol = 1e-12, maxit = 1000)
mod_polr <- MASS::polr(update(rhs, re78ord ~ .), data = dat, Hess = TRUE)

# Baseline-category logit; coefficients ordered as in `estfun()`.
probs_multinom <- function(beta, X) {
    B <- matrix(beta, ncol = ncol(X), byrow = TRUE)
    eta <- cbind(0, X %*% t(B))
    exp(eta) / rowSums(exp(eta))
}
beta_multinom <- as.vector(t(coef(mod_multinom)))
names(beta_multinom) <- colnames(sandwich::estfun(mod_multinom))

# Cumulative logit, P(Y <= k) = plogis(zeta_k - eta).
probs_polr <- function(beta, X) {
    b <- beta[colnames(X)[-1]]
    zeta <- beta[names(mod_polr$zeta)]
    cum <- stats::plogis(outer(-drop(X[, -1, drop = FALSE] %*% b), zeta, "+"))
    t(apply(cbind(0, cum, 1), 1, diff))
}
beta_polr <- c(mod_polr$coefficients, mod_polr$zeta)

# Influence function of the average probabilities over the rows in `subgroup`,
# with `treat` optionally set to a counterfactual value.
manual_avg_probs <- function(mod, beta, probs, subgroup = rep(TRUE, n), treat = NULL) {
    if_beta <- (sandwich::estfun(mod) %*% sandwich::bread(mod))[, names(beta)]
    d <- dat
    if (!is.null(treat)) {
        d$treat <- treat
    }
    Xd <- model.matrix(rhs, d)
    p_sub <- function(b) probs(b, Xd)[subgroup, , drop = FALSE]
    p <- p_sub(beta)
    theta <- colMeans(p)
    J <- numDeriv::jacobian(function(b) colMeans(p_sub(b)), beta)
    empirical <- matrix(0, n, ncol(p))
    empirical[subgroup, ] <- sweep(p, 2, theta) * n / sum(subgroup)
    phi <- empirical + if_beta %*% t(J)
    list(estimate = theta, std.error = sqrt(diag(crossprod(phi))) / n)
}

for (spec in list(
    list(mod = mod_multinom, beta = beta_multinom, probs = probs_multinom),
    list(mod = mod_polr, beta = beta_polr, probs = probs_polr)
)) {
    mod <- spec$mod

    # Average over the whole sample
    ref <- manual_avg_probs(mod, spec$beta, spec$probs)
    prd <- avg_predictions(mod, vcov = vcovUnconditional())
    expect_equivalent(prd$estimate, ref$estimate, tolerance = 1e-8)
    expect_equivalent(prd$std.error, ref$std.error, tolerance = 1e-6)

    # Counterfactual grid built on a subset of the model data
    treated <- dat$treat == 1
    ref <- manual_avg_probs(mod, spec$beta, spec$probs, subgroup = treated, treat = 0)
    prd <- avg_predictions(mod, variables = list(treat = 0),
        newdata = subset(treat == 1), vcov = vcovUnconditional())
    expect_equivalent(prd$estimate, ref$estimate, tolerance = 1e-8)
    expect_equivalent(prd$std.error, ref$std.error, tolerance = 1e-6)

    # A subset of the model data is the corresponding subgroup of `by`
    ref <- avg_comparisons(mod, variables = "treat", by = "treat", vcov = vcovUnconditional())
    ref <- ref[ref$treat == 1, ]
    att <- avg_comparisons(mod, variables = "treat", newdata = subset(treat == 1),
        vcov = vcovUnconditional())
    expect_equivalent(att$estimate, ref$estimate, tolerance = 1e-10)
    expect_equivalent(att$std.error, ref$std.error, tolerance = 1e-10)

    # The subset lacks some levels of `race`, so the grid is padded with rows
    # that are dropped from the estimates.
    ref <- avg_predictions(mod, by = "race", vcov = vcovUnconditional())
    ref <- ref[ref$race == "hispan", ]
    sub <- avg_predictions(mod, newdata = subset(race == "hispan"), vcov = vcovUnconditional())
    expect_equivalent(sub$estimate, ref$estimate, tolerance = 1e-10)
    expect_equivalent(sub$std.error, ref$std.error, tolerance = 1e-10)
}
