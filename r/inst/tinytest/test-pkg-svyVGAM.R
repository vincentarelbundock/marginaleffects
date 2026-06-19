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

# [Stata] svyset sdmvpsu [pweight=wtint2yr], strata(sdmvstra)
nhdes <- survey::svydesign(
    id = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTINT2YR,
    nest = TRUE,
    data = dat
)
nhdes <- subset(nhdes, RIDAGEYR > 22)
newdata_old <- dat[dat$old, , drop = FALSE]

# [Stata] svy, subpop(if ridageyr> 22): mlogit ridreth1 i.old##c.ridageyr c.dmdeduc, base(3)
mod_multinom <- svyVGAM::svy_vglm(
    RIDRETH1 ~ old * RIDAGEYR + DMDEDUC,
    family = VGAM::multinomial(refLevel = "C"),
    design = nhdes
)

stata_coef_multinom <- c(
    "(Intercept):1" = -0.83779609,
    "(Intercept):2" = 3.5550663,
    "(Intercept):3" = -0.24120008,
    "(Intercept):4" = -1.9636894,
    "oldTRUE:1" = 3.2827035,
    "oldTRUE:2" = -4.5499507,
    "oldTRUE:3" = 0.42530642,
    "oldTRUE:4" = 0.51513217,
    "RIDAGEYR:1" = 0.08737489,
    "RIDAGEYR:2" = -0.16357547,
    "RIDAGEYR:3" = 0.00148479,
    "RIDAGEYR:4" = 0.02551453,
    "DMDEDUC:1" = -1.3131213,
    "DMDEDUC:2" = -0.67931364,
    "DMDEDUC:3" = -0.56869728,
    "DMDEDUC:4" = -0.39883817,
    "oldTRUE:RIDAGEYR:1" = -0.13066959,
    "oldTRUE:RIDAGEYR:2" = 0.15558127,
    "oldTRUE:RIDAGEYR:3" = -0.01633275,
    "oldTRUE:RIDAGEYR:4" = -0.03003825
)
stata_se_multinom <- c(
    "(Intercept):1" = 1.1674071,
    "(Intercept):2" = 1.9742207,
    "(Intercept):3" = 1.6786402,
    "(Intercept):4" = 2.6587788,
    "oldTRUE:1" = 1.2831166,
    "oldTRUE:2" = 2.2424264,
    "oldTRUE:3" = 1.6875305,
    "oldTRUE:4" = 2.7605635,
    "RIDAGEYR:1" = 0.04053713,
    "RIDAGEYR:2" = 0.06794477,
    "RIDAGEYR:3" = 0.05975541,
    "RIDAGEYR:4" = 0.09784786,
    "DMDEDUC:1" = 0.14932844,
    "DMDEDUC:2" = 0.14721433,
    "DMDEDUC:3" = 0.0817063,
    "DMDEDUC:4" = 0.16761793,
    "oldTRUE:RIDAGEYR:1" = 0.04677293,
    "oldTRUE:RIDAGEYR:2" = 0.08046543,
    "oldTRUE:RIDAGEYR:3" = 0.06165714,
    "oldTRUE:RIDAGEYR:4" = 0.09937603
)
expect_equivalent(
    get_coef(mod_multinom)[names(stata_coef_multinom)],
    stata_coef_multinom,
    tolerance = 1e-7
)
expect_equivalent(
    sqrt(diag(get_vcov(mod_multinom)))[names(stata_se_multinom)],
    stata_se_multinom,
    tolerance = 1e-7
)

# [Stata] margins, noweights
stata_pred_unweighted <- data.frame(
    group = c("A", "B", "C", "D", "E"),
    estimate = c(0.1074637, 0.045062, 0.6698912, 0.122167, 0.0554161),
    std.error = c(0.0222727, 0.0073255, 0.0348598, 0.0190675, 0.0084162)
)
expect_warning({
    pred_unweighted <- avg_predictions(mod_multinom)
})
pred_unweighted <- pred_unweighted[
    match(stata_pred_unweighted$group, pred_unweighted$group),
]
expect_equivalent(
    pred_unweighted$estimate,
    stata_pred_unweighted$estimate,
    tolerance = 1e-7
)
expect_equivalent(
    pred_unweighted$std.error,
    stata_pred_unweighted$std.error,
    tolerance = 5e-7
)

# [Stata] margins
stata_pred_weighted <- data.frame(
    group = c("A", "B", "C", "D", "E"),
    estimate = c(0.0880047, 0.0417438, 0.6993513, 0.116644, 0.0542561),
    std.error = c(0.0188875, 0.0069072, 0.0324227, 0.0186244, 0.0078475)
)
pred_weighted <- avg_predictions(mod_multinom, wts = "WTINT2YR")
pred_weighted <- pred_weighted[
    match(stata_pred_weighted$group, pred_weighted$group),
]
expect_equivalent(
    pred_weighted$estimate,
    stata_pred_weighted$estimate,
    tolerance = 1e-7
)
expect_equivalent(
    pred_weighted$std.error,
    stata_pred_weighted$std.error,
    tolerance = 1e-7
)

# [Stata] margins r.old, subpop(if old) contrast(effects nowald)
stata_old_contrast <- data.frame(
    group = c("A", "B", "C", "D", "E"),
    estimate = c(-.3212141, .0323004, .2737438, .0291133, -.0139434),
    std.error = c(.1242732, .0098928, .168364, .0713081, .0915062)
)
old_contrast <- avg_comparisons(
    mod_multinom,
    variables = "old",
    newdata = newdata_old,
    wts = "WTINT2YR"
)
old_contrast <- old_contrast[
    match(stata_old_contrast$group, old_contrast$group),
]
expect_equivalent(
    old_contrast$estimate,
    stata_old_contrast$estimate,
    tolerance = 1e-7
)
expect_equivalent(
    old_contrast$std.error,
    stata_old_contrast$std.error,
    tolerance = 1e-7
)

# [Stata] svy, subpop(if ridageyr> 22): glm in_a_b i.old##c.ridageyr c.dmdeduc
mod_normal <- svyVGAM::svy_vglm(
    in_a_b ~ old * RIDAGEYR + DMDEDUC,
    family = VGAM::uninormal(),
    design = nhdes
)
stata_coef_normal <- c(
    "(Intercept):1" = 0.4622347,
    "oldTRUE" = 0.0573787,
    "RIDAGEYR" = 0.0011375,
    "DMDEDUC" = -0.1207603,
    "oldTRUE:RIDAGEYR" = -0.0037289
)
stata_se_normal <- c(
    "(Intercept):1" = 0.1695463,
    "oldTRUE" = 0.1497723,
    "RIDAGEYR" = 0.0049854,
    "DMDEDUC" = 0.0248927,
    "oldTRUE:RIDAGEYR" = 0.005365
)
expect_equivalent(
    get_coef(mod_normal)[names(stata_coef_normal)],
    stata_coef_normal,
    tolerance = 1e-7
)
expect_equivalent(
    sqrt(diag(get_vcov(mod_normal)))[names(stata_se_normal)],
    stata_se_normal,
    tolerance = 1e-7
)

# [Stata] margins, subpop(if old) dydx(dmdeduc)
normal_slope <- avg_slopes(
    mod_normal,
    variables = "DMDEDUC",
    newdata = newdata_old,
    wts = "WTINT2YR"
)
expect_equivalent(normal_slope$estimate, -0.1207603, tolerance = 5e-7)
expect_equivalent(normal_slope$std.error, 0.0248927, tolerance = 5e-7)

# [Stata] svy, subpop(if ridageyr> 22): logit in_a_b old##c.ridageyr dmdeduc
mod_logit <- suppressWarnings(svyVGAM::svy_vglm(
    in_a_b ~ old * RIDAGEYR + DMDEDUC,
    family = VGAM::binomialff(),
    design = nhdes
))

stata_coef_logit <- c(
    "(Intercept)" = 0.6209922,
    "oldTRUE" = 0.6784544,
    "RIDAGEYR" = 0.0078891,
    "DMDEDUC" = -0.9759996,
    "oldTRUE:RIDAGEYR" = -0.0359325
)
stata_se_logit <- c(
    "(Intercept)" = 0.997931,
    "oldTRUE" = 1.170115,
    "RIDAGEYR" = 0.0344935,
    "DMDEDUC" = 0.1065717,
    "oldTRUE:RIDAGEYR" = 0.0407567
)
expect_equivalent(
    get_coef(mod_logit)[names(stata_coef_logit)],
    stata_coef_logit,
    tolerance = 1e-7
)
expect_equivalent(
    sqrt(diag(get_vcov(mod_logit)))[names(stata_se_logit)],
    stata_se_logit,
    tolerance = 2e-5
)

# [Stata] margins i.old
stata_pred_by_old <- data.frame(
    group = c(FALSE, TRUE),
    estimate = c(.2163307, .1222764),
    std.error = c(.0793145, .0206125)
)
pred_by_old <- avg_predictions(
    mod_logit,
    variables = "old",
    wts = "WTINT2YR"
)
expect_equivalent(
    pred_by_old$estimate,
    stata_pred_by_old$estimate,
    tolerance = 1e-7
)
expect_equivalent(
    pred_by_old$std.error,
    stata_pred_by_old$std.error,
    tolerance = 1e-6
)
