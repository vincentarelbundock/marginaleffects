source("helpers.R")
using("marginaleffects")

requiet("sandwich")

# HC0 and HC1 follow the conventional sandwich multipliers. The cluster-count
# adjustment is independent of `type`, like vcovCL(cadjust = TRUE).
correction_inputs <- list(
    n = 20,
    k = 3,
    vcov = list(type = "HC0", cluster = NULL)
)
expect_equivalent(
    marginaleffects:::get_unconditional_correction(correction_inputs),
    1
)
correction_inputs$vcov$type <- "HC1"
expect_equivalent(
    marginaleffects:::get_unconditional_correction(correction_inputs),
    20 / 17
)
correction_inputs$vcov$cluster <- rep(seq_len(4), each = 5)
expect_equivalent(
    marginaleffects:::get_unconditional_correction(correction_inputs),
    (4 / 3) * (19 / 17)
)
correction_inputs$vcov$type <- "HC0"
expect_equivalent(
    marginaleffects:::get_unconditional_correction(correction_inputs),
    4 / 3
)

expect_identical(vcovUnconditional()$type, "HC0")

dat <- mtcars
dat$id <- seq_len(nrow(dat))
dat$cluster <- dat$cyl
dat$amf <- factor(dat$am)
dat$vsb <- dat$vs

mod_lm <- lm(mpg ~ amf + hp + wt, data = dat)
n <- stats::nobs(mod_lm)
k_lm <- ncol(sandwich::estfun(mod_lm))

p_lm_default <- avg_predictions(mod_lm, variables = "amf", vcov = "unconditional", df = Inf)
p_lm_hc0 <- avg_predictions(
    mod_lm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC0"),
    df = Inf
)
p_lm_hc1 <- avg_predictions(
    mod_lm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC1"),
    df = Inf
)
expect_equivalent(vcov(p_lm_default), vcov(p_lm_hc0), tolerance = 1e-8)
expect_equivalent(
    vcov(p_lm_hc1),
    vcov(p_lm_hc0) * n / (n - k_lm),
    tolerance = 1e-8
)

p_lm_cluster_hc0 <- avg_predictions(
    mod_lm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC0", cluster = ~cluster),
    df = Inf
)
p_lm_cluster_hc1 <- avg_predictions(
    mod_lm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC1", cluster = ~cluster),
    df = Inf
)
expect_equivalent(
    vcov(p_lm_cluster_hc1),
    vcov(p_lm_cluster_hc0) * (n - 1) / (n - k_lm),
    tolerance = 1e-8
)

# The user-facing inference df does not enter the HC covariance multiplier.
p_lm_df1 <- avg_predictions(
    mod_lm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC1"),
    df = 1
)
p_lm_df9 <- avg_predictions(
    mod_lm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC1"),
    df = 9
)
expect_equivalent(vcov(p_lm_df1), vcov(p_lm_df9), tolerance = 1e-8)
expect_equivalent(p_lm_df1$std.error, p_lm_df9$std.error, tolerance = 1e-8)
expect_equivalent(p_lm_df1$df, rep(1, nrow(p_lm_df1)))
expect_equivalent(p_lm_df9$df, rep(9, nrow(p_lm_df9)))

# With one observation per cluster, clustered HC1 collapses to iid HC1. HC0
# retains sandwich's independent G / (G - 1) cluster adjustment.
p_lm_unique_hc0 <- avg_predictions(
    mod_lm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC0", cluster = ~id),
    df = Inf
)
p_lm_unique_hc1 <- avg_predictions(
    mod_lm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC1", cluster = ~id),
    df = Inf
)
expect_equivalent(
    vcov(p_lm_unique_hc0),
    vcov(p_lm_hc0) * n / (n - 1),
    tolerance = 1e-8
)
expect_equivalent(vcov(p_lm_unique_hc1), vcov(p_lm_hc1), tolerance = 1e-8)

# GLMs use the same model-score correction as linear models.
mod_glm <- glm(vsb ~ amf + hp + wt, family = binomial, data = dat)
k_glm <- ncol(sandwich::estfun(mod_glm))
p_glm_hc0 <- avg_predictions(
    mod_glm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC0")
)
p_glm_hc1 <- avg_predictions(
    mod_glm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC1")
)
expect_equivalent(
    vcov(p_glm_hc1),
    vcov(p_glm_hc0) * n / (n - k_glm),
    tolerance = 1e-8
)

p_glm_cluster_hc0 <- avg_predictions(
    mod_glm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC0", cluster = ~cluster)
)
p_glm_cluster_hc1 <- avg_predictions(
    mod_glm,
    variables = "amf",
    vcov = vcovUnconditional(type = "HC1", cluster = ~cluster)
)
expect_equivalent(
    vcov(p_glm_cluster_hc1),
    vcov(p_glm_cluster_hc0) * (n - 1) / (n - k_glm),
    tolerance = 1e-8
)

# HC0 remains available for a saturated model; HC1 is undefined when n = k.
dat_saturated <- data.frame(y = c(1, 2, 3, 4), x = factor(1:4))
mod_saturated <- lm(y ~ x, data = dat_saturated)
expect_unconditional <- function(x) {
    expect_true(is.matrix(vcov(x)))
    expect_true(all(is.finite(vcov(x))))
}
expect_unconditional(avg_predictions(mod_saturated, vcov = "unconditional", df = Inf))
expect_error(
    avg_predictions(
        mod_saturated,
        vcov = vcovUnconditional(type = "HC1"),
        df = Inf
    ),
    pattern = "more observations than model estimating functions"
)

if (requiet("fixest")) {
    mod_fixest_fe <- fixest::feols(mpg ~ amf + hp + wt | cluster, data = dat)
    k_fixest <- ncol(mod_fixest_fe$scores)
    inputs_fixest <- list(
        n = mod_fixest_fe$nobs,
        k = k_fixest,
        vcov = list(type = "HC1", cluster = NULL)
    )
    expect_equivalent(
        marginaleffects:::get_unconditional_correction(inputs_fixest),
        mod_fixest_fe$nobs / (mod_fixest_fe$nobs - k_fixest)
    )
    inputs_fixest$vcov$cluster <- dat$cluster
    G <- length(unique(dat$cluster))
    expect_equivalent(
        marginaleffects:::get_unconditional_correction(inputs_fixest),
        (G / (G - 1)) *
            ((mod_fixest_fe$nobs - 1) / (mod_fixest_fe$nobs - k_fixest))
    )
}
