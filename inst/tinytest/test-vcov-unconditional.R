source("helpers.R")
using("marginaleffects")

# ===========================================================================
# vcov = "unconditional": influence-function variance estimator
# Hansen & Overgaard (2025, Metrika)
# ===========================================================================

requiet("sandwich")

# ---------------------------------------------------------------------------
# GLM (logistic): validate against {beeca} reference values
# ---------------------------------------------------------------------------
if (requiet("beeca")) {
    data01 <- trial01 |>
        transform(trtp = as.factor(trtp)) |>
        subset(!is.na(aval))
    fit <- glm(aval ~ trtp + bl_cov, family = "binomial", data = data01)

    # Reference from beeca::get_marginal_effect (Ye method)
    ye <- get_marginal_effect(
        object = fit, trt = "trtp", method = "Ye",
        contrast = "diff", reference = "0")

    # avg_comparisons with unconditional variance
    cmp <- avg_comparisons(fit, variables = "trtp", vcov = "unconditional")
    expect_equivalent(cmp$estimate, ye$marginal_est, tolerance = 1e-6)
    expect_equivalent(cmp$std.error, ye$marginal_se, tolerance = 0.01)

    # avg_predictions with unconditional variance
    pred <- avg_predictions(fit, variables = "trtp", vcov = "unconditional")
    expect_inherits(pred, "data.frame")
    expect_true(nrow(pred) == 2)
    expect_true(all(!is.na(pred$std.error)))
}


# ---------------------------------------------------------------------------
# GLM (logistic): mtcars (always available)
# ---------------------------------------------------------------------------
dat <- mtcars
dat$am <- as.factor(dat$am)
mod <- glm(vs ~ am + hp + wt, family = binomial, data = dat)

# avg_comparisons: unconditional SE should differ from default (delta method)
cmp_delta <- avg_comparisons(mod, variables = "am")
cmp_uncond <- avg_comparisons(mod, variables = "am", vcov = "unconditional")
expect_inherits(cmp_uncond, "data.frame")
expect_true(nrow(cmp_uncond) == 1)
expect_true(!is.na(cmp_uncond$std.error))
# Estimates should be identical (same point estimate)
expect_equivalent(cmp_delta$estimate, cmp_uncond$estimate, tolerance = 1e-10)
# Standard errors should generally differ
expect_true(abs(cmp_delta$std.error - cmp_uncond$std.error) > 1e-8)

# avg_predictions: unconditional SE
pred_delta <- avg_predictions(mod, variables = "am")
pred_uncond <- avg_predictions(mod, variables = "am", vcov = "unconditional")
expect_inherits(pred_uncond, "data.frame")
expect_true(nrow(pred_uncond) == 2)
expect_true(all(!is.na(pred_uncond$std.error)))
# Estimates should be identical
expect_equivalent(pred_delta$estimate, pred_uncond$estimate, tolerance = 1e-10)


# ---------------------------------------------------------------------------
# LM: unconditional SE
# ---------------------------------------------------------------------------
mod_lm <- lm(mpg ~ am + hp + wt, data = mtcars)

cmp_lm_delta <- avg_comparisons(mod_lm, variables = "am")
cmp_lm_uncond <- avg_comparisons(mod_lm, variables = "am", vcov = "unconditional")
expect_inherits(cmp_lm_uncond, "data.frame")
expect_true(nrow(cmp_lm_uncond) == 1)
expect_true(!is.na(cmp_lm_uncond$std.error))
expect_equivalent(cmp_lm_delta$estimate, cmp_lm_uncond$estimate, tolerance = 1e-10)

pred_lm_delta <- avg_predictions(mod_lm, variables = "am")
pred_lm_uncond <- avg_predictions(mod_lm, variables = "am", vcov = "unconditional")
expect_inherits(pred_lm_uncond, "data.frame")
expect_true(nrow(pred_lm_uncond) == 2)
expect_true(all(!is.na(pred_lm_uncond$std.error)))
expect_equivalent(pred_lm_delta$estimate, pred_lm_uncond$estimate, tolerance = 1e-10)


# ---------------------------------------------------------------------------
# Multi-level treatment (> 2 arms)
# ---------------------------------------------------------------------------
dat3 <- mtcars
dat3$gear <- as.factor(dat3$gear)
mod3 <- glm(vs ~ gear + hp + wt, family = binomial, data = dat3)

# 3 levels: should produce 2 contrasts (4 vs 3, 5 vs 3)
cmp3 <- avg_comparisons(mod3, variables = "gear", vcov = "unconditional")
expect_inherits(cmp3, "data.frame")
expect_true(nrow(cmp3) == 2)
expect_true(all(!is.na(cmp3$std.error)))

pred3 <- avg_predictions(mod3, variables = "gear", vcov = "unconditional")
expect_inherits(pred3, "data.frame")
expect_true(nrow(pred3) == 3)
expect_true(all(!is.na(pred3$std.error)))


# ---------------------------------------------------------------------------
# Informative errors: unsupported model class
# ---------------------------------------------------------------------------
if (requiet("survival")) {
    dat_surv <- survival::lung
    dat_surv$sex <- as.factor(dat_surv$sex)
    mod_cox <- survival::coxph(survival::Surv(time, status) ~ sex + age, data = dat_surv)
    expect_error(
        avg_comparisons(mod_cox, variables = "sex", vcov = "unconditional"),
        pattern = "only supported for lm and glm"
    )
}


# ---------------------------------------------------------------------------
# Informative errors: comparison != "difference"
# ---------------------------------------------------------------------------
expect_error(
    avg_comparisons(mod, variables = "am", vcov = "unconditional", comparison = "ratio"),
    pattern = "only supports comparison"
)

expect_error(
    avg_comparisons(mod, variables = "am", vcov = "unconditional", comparison = "lnratio"),
    pattern = "only supports comparison"
)


# ---------------------------------------------------------------------------
# Informative errors: slopes() not supported (no variables in the right form)
# ---------------------------------------------------------------------------
expect_error(
    avg_slopes(mod, vcov = "unconditional"),
    pattern = "unconditional"
)


rm(list = ls())
