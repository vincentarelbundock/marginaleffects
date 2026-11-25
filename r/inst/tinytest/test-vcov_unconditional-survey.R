source("helpers.R")
using("marginaleffects")

if (!requiet("survey")) exit_file("survey not installed")

# Unconditional standard errors for svyglm models use the design-based
# variance of the linearized estimator (Binder, 1983), not an i.i.d. sum of
# squared influence values. The reference below computes the linearization by
# hand for average predictions at each level of `stype`, weighted by the
# survey weights, and passes it to survey::svyrecvar() directly. `w` holds the
# averaging weights: the survey weights, or equal weights for the default
# unweighted average.
binder_se <- function(model, design, levels, w = weights(design)) {
    b <- coef(model)
    infl <- attr(model, "influence")
    d <- model.frame(design)
    z <- sapply(levels, function(l) {
        d$stype <- factor(l, levels = levels(d$stype))
        X <- model.matrix(delete.response(terms(model)), d)
        p <- drop(X %*% b)
        theta <- sum(w * p) / sum(w)
        w * (p - theta) / sum(w) + drop(infl %*% (colSums(w * X) / sum(w)))
    })
    V <- svyrecvar(z, design$cluster, design$strata, design$fpc, postStrata = design$postStrata)
    sqrt(diag(V))
}

data("api", package = "survey")
designs <- list(
    cluster_fpc = svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc),
    strata_cluster = svydesign(id = ~dnum, strata = ~stype, weights = ~pw, data = apistrat, nest = TRUE)
)
designs$domain <- subset(designs$strata_cluster, sch.wide == "Yes")
designs$calibrated <- calibrate(
    designs$strata_cluster,
    ~stype,
    c(`(Intercept)` = 6194, stypeH = 755, stypeM = 1018)
)
# The totals above are those of the stratified design, so calibration leaves
# its weights unchanged. Calibrating the cluster sample moves every weight.
designs$calibrated_cluster <- calibrate(
    designs$cluster_fpc,
    ~stype + api99,
    c(`(Intercept)` = 6194, stypeH = 755, stypeM = 1018, api99 = 3914069)
)
expect_true(all(abs(weights(designs$calibrated_cluster) / weights(designs$cluster_fpc) - 1) > 1e-3))

for (d in designs) {
    mod <- svyglm(api00 ~ ell + meals + stype, design = d, influence = TRUE)
    p <- avg_predictions(mod, variables = "stype", wts = "(weights)", vcov = "unconditional")
    expect_equivalent(p$std.error, binder_se(mod, d, as.character(p$stype)), tolerance = 1e-8)
}

# A domain of a calibrated design keeps the observations outside the domain,
# with a weight of zero
design_zero <- subset(designs$calibrated_cluster, sch.wide == "Yes")
expect_equal(nrow(design_zero), nrow(apiclus1))
expect_true(any(weights(design_zero) == 0))
mod <- suppressWarnings(svyglm(api00 ~ ell + meals + stype, design = design_zero, influence = TRUE))
p <- avg_predictions(mod, variables = "stype", wts = "(weights)", vcov = "unconditional")
expect_equivalent(p$std.error, binder_se(mod, design_zero, as.character(p$stype)), tolerance = 1e-8)

# `wts` must be supplied explicitly; `wts = TRUE` uses the survey weights
for (d in designs[c("cluster_fpc", "strata_cluster", "domain")]) {
    mod <- svyglm(api00 ~ ell + meals + stype, design = d, influence = TRUE)
    expect_error(
        avg_predictions(mod, variables = "stype", vcov = "unconditional"),
        pattern = "requires the `wts` argument"
    )
    p_true <- avg_predictions(mod, variables = "stype", wts = TRUE, vcov = "unconditional")
    p_name <- avg_predictions(mod, variables = "stype", wts = "(weights)", vcov = "unconditional")
    expect_equivalent(p_true$std.error, p_name$std.error, tolerance = 1e-12)
}

# With a one-stage cluster sample, ignoring the clusters makes the standard
# errors far too small
mod <- svyglm(api00 ~ ell + meals + stype, design = designs$cluster_fpc)
p <- avg_predictions(mod, variables = "stype", wts = "(weights)", vcov = "unconditional")
design_iid <- svydesign(id = ~1, weights = ~pw, data = apiclus1)
mod_iid <- svyglm(api00 ~ ell + meals + stype, design = design_iid, influence = TRUE)
expect_true(all(p$std.error > 1.5 * binder_se(mod_iid, design_iid, as.character(p$stype))))

# HC1 applies the model degrees-of-freedom factor to the design-based variance
p_hc1 <- avg_predictions(mod, variables = "stype", wts = "(weights)", vcov = vcovUnconditional("HC1"))
n <- nrow(apiclus1)
k <- length(coef(mod))
expect_equivalent(p_hc1$std.error, p$std.error * sqrt(n / (n - k)), tolerance = 1e-8)

# The design already identifies the clusters
expect_error(
    avg_predictions(mod, wts = "(weights)", vcov = vcovUnconditional(cluster = ~dnum)),
    pattern = "cannot be supplied"
)
