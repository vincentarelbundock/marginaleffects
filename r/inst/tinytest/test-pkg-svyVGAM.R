source("helpers.R")
using("marginaleffects")

if (!requiet("survey")) exit_file("survey")
if (!requiet("svyVGAM")) exit_file("svyVGAM")
if (!requiet("VGAM")) exit_file("VGAM")

data("nhanes_sxq", package = "svyVGAM")
dat <- nhanes_sxq
dat$RIDRETH1 <- factor(
    dat$RIDRETH1,
    levels = seq(5),
    labels = c("A", "B", "C", "D", "E")
)
dat$in_a_b <- dat$RIDRETH1 %in% c("A", "B")
dat$old <- dat$RIDAGEYR > 30

nhdes <- survey::svydesign(
    id = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTINT2YR,
    nest = TRUE,
    data = dat
)
nhdes <- subset(nhdes, RIDAGEYR > 22)
newdata_old <- dat[dat$old, , drop = FALSE]


# svy: mlogit RIDRETH1 old##c.RIDAGEYR DMDEDUC, base(3)
mod_multinom <- svyVGAM::svy_vglm(
    RIDRETH1 ~ old * RIDAGEYR + DMDEDUC,
    family = VGAM::multinomial(refLevel = 3),
    design = nhdes
)

stata_coef_multinom <- c(
    "(Intercept):1" = -0.8377961,
    "(Intercept):2" = 3.5550664,
    "(Intercept):3" = -0.2412002,
    "(Intercept):4" = -1.9636894,
    "oldTRUE:1" = 3.2827037,
    "oldTRUE:2" = -4.5499508,
    "oldTRUE:3" = 0.4253065,
    "oldTRUE:4" = 0.5151322,
    "RIDAGEYR:1" = 0.0873749,
    "RIDAGEYR:2" = -0.1635755,
    "RIDAGEYR:3" = 0.0014848,
    "RIDAGEYR:4" = 0.0255145,
    "DMDEDUC:1" = -1.3131213,
    "DMDEDUC:2" = -0.6793136,
    "DMDEDUC:3" = -0.5686973,
    "DMDEDUC:4" = -0.3988382,
    "oldTRUE:RIDAGEYR:1" = -0.1306696,
    "oldTRUE:RIDAGEYR:2" = 0.1555813,
    "oldTRUE:RIDAGEYR:3" = -0.0163328,
    "oldTRUE:RIDAGEYR:4" = -0.0300383
)
stata_se_multinom <- c(
    "(Intercept):1" = 1.1674071,
    "(Intercept):2" = 1.9742207,
    "(Intercept):3" = 1.6786402,
    "(Intercept):4" = 2.6587787,
    "oldTRUE:1" = 1.2831166,
    "oldTRUE:2" = 2.2424264,
    "oldTRUE:3" = 1.6875304,
    "oldTRUE:4" = 2.7605635,
    "RIDAGEYR:1" = 0.0405371,
    "RIDAGEYR:2" = 0.0679448,
    "RIDAGEYR:3" = 0.0597554,
    "RIDAGEYR:4" = 0.0978479,
    "DMDEDUC:1" = 0.1493284,
    "DMDEDUC:2" = 0.1472143,
    "DMDEDUC:3" = 0.0817063,
    "DMDEDUC:4" = 0.1676179,
    "oldTRUE:RIDAGEYR:1" = 0.0467729,
    "oldTRUE:RIDAGEYR:2" = 0.0804654,
    "oldTRUE:RIDAGEYR:3" = 0.0616571,
    "oldTRUE:RIDAGEYR:4" = 0.0993760
)
expect_equivalent(
    get_coef(mod_multinom)[names(stata_coef_multinom)],
    stata_coef_multinom,
    tolerance = 1e-5
)
expect_equivalent(
    sqrt(diag(get_vcov(mod_multinom)))[names(stata_se_multinom)],
    stata_se_multinom,
    tolerance = 1e-5
)

# Stata: margins
stata_pred_weighted <- data.frame(
    group = c("A", "B", "C", "D", "E"),
    estimate = c(0.0888732, 0.0493738, 0.6902235, 0.1172998, 0.0542297),
    std.error = c(0.0186987, 0.0091566, 0.0346247, 0.0197569, 0.0082793)
)
pred_weighted <- avg_predictions(mod_multinom, wts = "WTINT2YR")
pred_weighted <- pred_weighted[match(stata_pred_weighted$group, pred_weighted$group), ]
expect_equivalent(pred_weighted$estimate, stata_pred_weighted$estimate, tolerance = 5e-3)
expect_equivalent(pred_weighted$std.error, stata_pred_weighted$std.error, tolerance = 5e-3)

# Stata: margins r.old, subpop(if old) contrast(effects nowald)
stata_old_contrast <- data.frame(
    group = c("A", "B", "C", "D", "E"),
    estimate = c(-0.3212141, 0.0323004, 0.2737438, 0.0291133, -0.0139434),
    std.error = c(0.1242732, 0.0098928, 0.1683640, 0.0713081, 0.0915062)
)
old_contrast <- avg_comparisons(
    mod_multinom,
    variables = "old",
    newdata = newdata_old,
    wts = "WTINT2YR"
)
old_contrast <- old_contrast[match(stata_old_contrast$group, old_contrast$group), ]
expect_equivalent(old_contrast$estimate, stata_old_contrast$estimate, tolerance = 2e-2)
expect_equivalent(old_contrast$std.error, stata_old_contrast$std.error, tolerance = 2e-2)


# svy: glm in_a_b old##c.RIDAGEYR DMDEDUC
mod_normal <- svyVGAM::svy_vglm(
    in_a_b ~ old * RIDAGEYR + DMDEDUC,
    family = VGAM::uninormal(),
    design = nhdes
)

normal_slope <- avg_slopes(
    mod_normal,
    variables = "DMDEDUC",
    newdata = newdata_old,
    wts = "WTINT2YR"
)
expect_equivalent(normal_slope$estimate, -0.1207603, tolerance = 1e-5)
expect_equivalent(normal_slope$std.error, 0.0248927, tolerance = 1e-5)


# svy: logit in_a_b old##c.RIDAGEYR DMDEDUC
mod_logit <- suppressWarnings(svyVGAM::svy_vglm(
    in_a_b ~ old * RIDAGEYR + DMDEDUC,
    family = VGAM::binomialff(),
    design = nhdes
))

stata_coef_logit <- c(
    "(Intercept)" = 0.6209923,
    "oldTRUE" = 0.6784544,
    "RIDAGEYR" = 0.0078891,
    "DMDEDUC" = -0.9759996,
    "oldTRUE:RIDAGEYR" = -0.0359325
)
stata_se_logit <- c(
    "(Intercept)" = 0.9979309,
    "oldTRUE" = 1.1701150,
    "RIDAGEYR" = 0.0344935,
    "DMDEDUC" = 0.1065717,
    "oldTRUE:RIDAGEYR" = 0.0407567
)
expect_equivalent(
    get_coef(mod_logit)[names(stata_coef_logit)],
    stata_coef_logit,
    tolerance = 1e-5
)
expect_equivalent(
    sqrt(diag(get_vcov(mod_logit)))[names(stata_se_logit)],
    stata_se_logit,
    tolerance = 1e-5
)
