source("inst/tinytest/helpers.R")
using("marginaleffects")
source("get_vcov_unconditional.R")

# ===========================================================================
# vcov_unconditional(): influence-function variance estimator
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

    # avg_comparisons then vcov_unconditional
    cmp <- avg_comparisons(fit, variables = "trtp")
    cmp_uncond <- vcov_unconditional(cmp)
    expect_equivalent(cmp_uncond$estimate, ye$marginal_est, tolerance = 1e-6)
    expect_equivalent(cmp_uncond$std.error, ye$marginal_se, tolerance = 0.01)

    # avg_predictions then vcov_unconditional
    pred <- avg_predictions(fit, variables = "trtp")
    pred_uncond <- vcov_unconditional(pred)
    expect_inherits(pred_uncond, "data.frame")
    expect_true(nrow(pred_uncond) == 2)
    expect_true(all(!is.na(pred_uncond$std.error)))
}


# ---------------------------------------------------------------------------
# GLM (logistic): mtcars (always available)
# ---------------------------------------------------------------------------
dat <- mtcars
dat$am <- as.factor(dat$am)
mod <- glm(vs ~ am + hp + wt, family = binomial, data = dat)

# avg_comparisons: unconditional SE should differ from default (delta method)
cmp_delta <- avg_comparisons(mod, variables = "am")
cmp_uncond <- vcov_unconditional(cmp_delta)
expect_inherits(cmp_uncond, "data.frame")
expect_true(nrow(cmp_uncond) == 1)
expect_true(!is.na(cmp_uncond$std.error))
# Estimates should be identical (same point estimate)
expect_equivalent(cmp_delta$estimate, cmp_uncond$estimate, tolerance = 1e-10)
# Standard errors should generally differ
expect_true(abs(cmp_delta$std.error - cmp_uncond$std.error) > 1e-8)

# avg_predictions: unconditional SE
pred_delta <- avg_predictions(mod, variables = "am")
pred_uncond <- vcov_unconditional(pred_delta)
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
cmp_lm_uncond <- vcov_unconditional(cmp_lm_delta)
expect_inherits(cmp_lm_uncond, "data.frame")
expect_true(nrow(cmp_lm_uncond) == 1)
expect_true(!is.na(cmp_lm_uncond$std.error))
expect_equivalent(cmp_lm_delta$estimate, cmp_lm_uncond$estimate, tolerance = 1e-10)

pred_lm_delta <- avg_predictions(mod_lm, variables = "am")
pred_lm_uncond <- vcov_unconditional(pred_lm_delta)
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
cmp3 <- vcov_unconditional(avg_comparisons(mod3, variables = "gear"))
expect_inherits(cmp3, "data.frame")
expect_true(nrow(cmp3) == 2)
expect_true(all(!is.na(cmp3$std.error)))

pred3 <- vcov_unconditional(avg_predictions(mod3, variables = "gear"))
expect_inherits(pred3, "data.frame")
expect_true(nrow(pred3) == 3)
expect_true(all(!is.na(pred3$std.error)))


# ---------------------------------------------------------------------------
# Subgroup estimation: avg_comparisons with by = "group_var"
# ---------------------------------------------------------------------------
dat_by <- mtcars
dat_by$am <- as.factor(dat_by$am)
dat_by$vs <- as.factor(dat_by$vs)
mod_by <- glm(mpg ~ am + vs + hp, data = dat_by)

# avg_comparisons with by = "vs": ATE of am within each vs group
cmp_by <- vcov_unconditional(avg_comparisons(mod_by, variables = "am", by = "vs"))
expect_inherits(cmp_by, "data.frame")
expect_true(nrow(cmp_by) == 2)
expect_true(all(!is.na(cmp_by$std.error)))

# Compare to overall ATE (no by): different SEs
cmp_overall <- vcov_unconditional(avg_comparisons(mod_by, variables = "am"))
expect_true(nrow(cmp_overall) == 1)
# The by-group SEs should not match the overall SE
expect_true(all(cmp_by$std.error != cmp_overall$std.error))

# Point estimates should match delta method
cmp_by_delta <- avg_comparisons(mod_by, variables = "am", by = "vs")
expect_equivalent(cmp_by$estimate, cmp_by_delta$estimate, tolerance = 1e-10)


# ---------------------------------------------------------------------------
# Subgroup estimation: avg_predictions with by = c(trt, group)
# ---------------------------------------------------------------------------
pred_by <- vcov_unconditional(
    avg_predictions(mod_by, variables = "am", by = c("am", "vs")))
expect_inherits(pred_by, "data.frame")
expect_true(nrow(pred_by) == 4) # 2 am levels x 2 vs groups
expect_true(all(!is.na(pred_by$std.error)))

# Point estimates match delta method
pred_by_delta <- avg_predictions(mod_by, variables = "am", by = c("am", "vs"))
expect_equivalent(pred_by$estimate, pred_by_delta$estimate, tolerance = 1e-10)


# ---------------------------------------------------------------------------
# Subgroup estimation: GLM (logistic) with by
# ---------------------------------------------------------------------------
dat_glm_by <- mtcars
dat_glm_by$am <- as.factor(dat_glm_by$am)
dat_glm_by$cyl <- as.factor(dat_glm_by$cyl)
mod_glm_by <- glm(vs ~ am + cyl + hp, family = binomial, data = dat_glm_by)

# ATE of am by cyl group
cmp_glm_by <- vcov_unconditional(
    avg_comparisons(mod_glm_by, variables = "am", by = "cyl"))
expect_inherits(cmp_glm_by, "data.frame")
expect_true(nrow(cmp_glm_by) == 3) # 3 cyl levels
expect_true(all(!is.na(cmp_glm_by$std.error)))

# Point estimates match delta method
cmp_glm_by_delta <- avg_comparisons(mod_glm_by, variables = "am", by = "cyl")
expect_equivalent(cmp_glm_by$estimate, cmp_glm_by_delta$estimate, tolerance = 1e-10)


# ---------------------------------------------------------------------------
# Informative errors: unsupported model class
# ---------------------------------------------------------------------------
if (requiet("survival")) {
    dat_surv <- survival::lung
    dat_surv$sex <- as.factor(dat_surv$sex)
    mod_cox <- survival::coxph(survival::Surv(time, status) ~ sex + age, data = dat_surv)
    expect_error(
        vcov_unconditional(avg_comparisons(mod_cox, variables = "sex")),
        pattern = "only supported for lm and glm"
    )
}


# ---------------------------------------------------------------------------
# Informative errors: comparison != "difference"
# ---------------------------------------------------------------------------
expect_error(
    vcov_unconditional(avg_comparisons(mod, variables = "am", comparison = "ratio")),
    pattern = "only supports comparison"
)

expect_error(
    vcov_unconditional(avg_comparisons(mod, variables = "am", comparison = "lnratio")),
    pattern = "only supports comparison"
)


# ---------------------------------------------------------------------------
# Informative errors: not a marginaleffects object
# ---------------------------------------------------------------------------
expect_error(
    vcov_unconditional(data.frame(x = 1)),
    pattern = "marginaleffects object"
)


# ---------------------------------------------------------------------------
# Informative errors: custom newdata
# ---------------------------------------------------------------------------
expect_error(
    vcov_unconditional(avg_predictions(mod, variables = "am", newdata = mtcars[1:10, ])),
    pattern = "original model dataset"
)


# ---------------------------------------------------------------------------
# CIs and test statistics are recomputed
# ---------------------------------------------------------------------------
cmp_orig <- avg_comparisons(mod, variables = "am")
cmp_unc <- vcov_unconditional(cmp_orig)
# conf.low and conf.high should exist and differ from original
expect_true(all(c("conf.low", "conf.high", "statistic", "p.value") %in% names(cmp_unc)))
expect_true(cmp_orig$conf.low != cmp_unc$conf.low)


rm(list = ls())
