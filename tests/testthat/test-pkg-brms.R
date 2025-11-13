# TODO: 744, 745, 749

# HPD tests against emmeans, which uses HDI, but our default is ETI
# HDI is implemented specifically for these tests
# https://github.com/vincentarelbundock/marginaleffects/issues/240
# exit_file("not sure why broken")
testthat::skip_if(!EXPENSIVE, "EXPENSIVE")
testthat::skip_if(ON_WINDOWS, "on windows")
testthat::skip_if(getRversion() < "4.1.0", "R 4.1.0")
options("marginaleffects_posterior_interval" = "hdi")
testthat::skip_if_not_installed("brms")
requiet("brms")
testthat::skip_if_not_installed("emmeans")
requiet("emmeans")
testthat::skip_if_not_installed("mice")
requiet("mice")
testthat::skip_if_not_installed("broom")
requiet("broom")
testthat::skip_if_not_installed("posterior")
requiet("posterior")
testthat::skip_if_not_installed("data.table")
requiet("data.table")
testthat::skip_if_not_installed("brmsmargins")
requiet("brmsmargins")
tol <- 0.0001
tol_se <- 0.001


# load models
brms_numeric <- readRDS(testing_path("modelarchive/data/brms_numeric.rds"))
brms_numeric2 <- readRDS(testing_path("modelarchive/data/brms_numeric2.rds"))
brms_character <- readRDS(testing_path("modelarchive/data/brms_character.rds"))
brms_factor <- readRDS(testing_path("modelarchive/data/brms_factor.rds"))
brms_factor_formula <- readRDS(testing_path("modelarchive/data/brms_factor_formula.rds"))
brms_interaction <- readRDS(testing_path("modelarchive/data/brms_interaction.rds"))
brms_logical <- readRDS(testing_path("modelarchive/data/brms_logical.rds"))
brms_epi <- readRDS(testing_path("modelarchive/data/brms_epi.rds"))
brms_cumulative_random <- readRDS(testing_path("modelarchive/data/brms_cumulative_random.rds"))
brms_monotonic <- readRDS(testing_path("modelarchive/data/brms_monotonic.rds"))
brms_monotonic_factor <- readRDS(testing_path("modelarchive/data/brms_monotonic_factor.rds"))
brms_vdem <- readRDS(testing_path("modelarchive/data/brms_vdem.rds"))
brms_lognormal_hurdle <- readRDS(testing_path("modelarchive/data/brms_lognormal_hurdle.rds"))
brms_lognormal_hurdle2 <- readRDS(testing_path("modelarchive/data/brms_lognormal_hurdle2.rds"))
brms_binomial <- readRDS(testing_path("modelarchive/data/brms_binomial.rds"))
brms_mv_1 <- readRDS(testing_path("modelarchive/data/brms_mv_1.rds"))
brms_vdem <- readRDS(testing_path("modelarchive/data/brms_vdem.rds"))
brms_ordinal_1 <- readRDS(testing_path("modelarchive/data/brms_ordinal_1.rds"))
brms_categorical_1 <- readRDS(testing_path("modelarchive/data/brms_categorical_1.rds"))
brms_logit_re <- readRDS(testing_path("modelarchive/data/brms_logit_re.rds"))
brms_mixed_3 <- readRDS(testing_path("modelarchive/data/brms_mixed_3.rds"))
brms_kurz <- readRDS(testing_path("modelarchive/data/brms_kurz.rds"))
brms_inhaler_cat <- readRDS(testing_path("modelarchive/data/brms_inhaler_cat.rds"))
brms_poisson <- readRDS(testing_path("modelarchive/data/brms_poisson.rds"))
brms_issue500 <- readRDS(testing_path("modelarchive/data/brms_issue500.rds"))
brms_issue576 <- readRDS(testing_path("modelarchive/data/brms_issue576.rds"))
brms_issue639 <- readRDS(testing_path("modelarchive/data/brms_issue639.rds"))
brms_issue782 <- readRDS(testing_path("modelarchive/data/brms_issue782.rds"))
brms_issue1006 <- readRDS(testing_path("modelarchive/data/brms_issue1006.rds"))
brms_categorical_2_num <- readRDS(testing_path("modelarchive/data/brms_categorical_2_num.rds"))


# average marginal effects brmsmargins
options("marginaleffects_posterior_interval" = "eti")
h <- 5e-5
bm <- brmsmargins(
    brms_numeric,
    add = data.frame(hp = c(0, 0 + h)),
    contrasts = cbind("AME MPG" = c(-1 / h, 1 / h)),
    CI = 0.95,
    CIType = "ETI"
)
bm <- data.frame(bm$ContrastSummary)
mfx <- avg_slopes(brms_numeric)
expect_equal(mean(get_draws(mfx)$draw), bm$M, tolerance = tol, ignore_attr = TRUE)
expect_equal(mfx$conf.low, bm$LL, tolerance = tol, ignore_attr = TRUE)
expect_equal(mfx$conf.high, bm$UL, tolerance = tol, ignore_attr = TRUE)

options("marginaleffects_posterior_interval" = "hdi")

# marginaleffects vs. emmeans
mfx <- avg_slopes(
    brms_numeric2,
    newdata = datagrid(mpg = 20, hp = 100),
    variables = "mpg",
    type = "link"
)

em <- emtrends(brms_numeric2, ~mpg, "mpg", at = list(mpg = 20, hp = 100))
em <- tidy(em)
expect_equal(mfx$estimate, em$mpg.trend, ignore_attr = TRUE)
expect_equal(mfx$conf.low, em$lower.HPD, ignore_attr = TRUE)
expect_equal(mfx$conf.high, em$upper.HPD, ignore_attr = TRUE)
# tolerance is less good for back-transformed response
mfx <- avg_slopes(brms_numeric2, newdata = datagrid(mpg = 20, hp = 100), variables = "mpg", type = "response")
em <- emtrends(brms_numeric2, ~mpg, "mpg", at = list(mpg = 20, hp = 100), regrid = "response")
em <- tidy(em)
expect_equal(mfx$estimate, em$mpg.trend, tolerance = .1, ignore_attr = TRUE)
expect_equal(mfx$conf.low, em$lower.HPD, tolerance = .01, ignore_attr = TRUE)
expect_equal(mfx$conf.high, em$upper.HPD, tolerance = .1, ignore_attr = TRUE)


# brms: cumulative: marginaleffects: no validity
expect_slopes2(brms_cumulative_random, se = FALSE)


# brms: logical regressor
mfx <- slopes(brms_logical)
expect_s3_class(mfx, "marginaleffects")
expect_equal(nrow(mfx), nrow(components(mfx, "draws")), ignore_attr = TRUE)


## Not sure what the intent of those tests are, and the first one fails
#
# # predictions: hypothetical group
# nd <- suppressWarnings(datagrid(model = brms_mixed_3, grp = 4, subgrp = 12))
# nd$Subject <- 1000000
# set.seed(1024)
# p1 <- predictions(brms_mixed_3, newdata = nd, allow_new_levels = TRUE)
# set.seed(1024)
# p2 <- predictions(brms_mixed_3, newdata = nd, allow_new_levels = TRUE, sample_new_levels = "gaussian")
# set.seed(1024)
# p3 <- predictions(brms_mixed_3, newdata = nd, allow_new_levels = TRUE, sample_new_levels = "uncertainty")
# expect_false(any(p1$estimate == p2$estimate))
# expect_equal(p1, p3, ignore_attr = TRUE)
# expect_s3_class(get_draws(p3), "data.frame")

# predictions w/ random effects
set.seed(123)
tmp <- get_dataset("sleepstudy", "lme4")
tmp$grp <- sample(1:5, size = 180, replace = TRUE)
tmp$cat <- as.factor(sample(1:5, size = 180, replace = TRUE))
tmp$Reaction_d <-
    ifelse(tmp$Reaction < median(tmp$Reaction), 0, 1)
tmp <- tmp |>
    dplyr::group_by(grp) |>
    dplyr::mutate(subgrp = sample(1:15, size = dplyr::n(), replace = TRUE))
w <- apply(posterior_linpred(brms_mixed_3), 2, stats::median)
x <- get_predict(brms_mixed_3, newdata = tmp, type = "link")
y <- predictions(brms_mixed_3, type = "link")
expect_equal(w, x$estimate, ignore_attr = TRUE)
expect_equal(w, y$estimate, ignore_attr = TRUE)

# response
w <- apply(posterior_epred(brms_mixed_3), 2, stats::median)
x <- get_predict(brms_mixed_3, type = "response")
y <- predictions(brms_mixed_3, type = "response")
expect_equal(w, x$estimate, ignore_attr = TRUE)
expect_equal(w, y$estimate, ignore_attr = TRUE)

# no random effects
w1 <- apply(posterior_epred(brms_mixed_3), 2, stats::median)
w2 <- apply(posterior_epred(brms_mixed_3, re_formula = NA), 2, stats::median)
x <- get_predict(brms_mixed_3, re_formula = NA, type = "response")
y <- predictions(brms_mixed_3, re_formula = NA, type = "response")
expect_true(all(w1 != w2))
expect_equal(w2, x$estimate, ignore_attr = TRUE)
expect_equal(w2, y$estimate, ignore_attr = TRUE)


# brms: cumulative: predictions: no validity
options(marginaleffects_safe = TRUE)
set.seed(1024)
p1 <- predictions(brms_cumulative_random)
p2 <- predictions(brms_cumulative_random, re_formula = NA)
expect_true(mean(p1$conf.low < p2$conf.low) > .95) # tolerance
expect_true(mean(p1$conf.high > p2$conf.high) > .99) # tolerance
expect_warning(predictions(brms_cumulative_random, include_random = FALSE)) # only for lme4
options(marginaleffects_safe = FALSE)

# marginaleffects: ordinal no validity
expect_slopes2(brms_ordinal_1, se = FALSE)


# predict new unit: no validity
dat1 <- dat2 <- datagrid(model = brms_epi)
dat2$patient <- 9999
set.seed(1024)
mfx1 <- slopes(brms_epi, newdata = dat1)
set.seed(1024)
mfx2 <- slopes(brms_epi, newdata = dat2, allow_new_levels = TRUE)
expect_false(any(mfx1$estimate == mfx2$estimate))


# tidy()
dat <- mtcars
dat$logic <- as.logical(dat$vs)
dat$cyl_fac <- as.factor(dat$cyl)
dat$cyl_cha <- as.character(dat$cyl)
ti <- avg_slopes(brms_factor, newdata = dat)
expect_s3_class(ti, "data.frame")
expect_true(nrow(ti) == 3)
expect_true(ncol(ti) >= 5)
expect_true(all(c("term", "estimate", "conf.low") %in% colnames(ti)))


# predictions: no validity
# simple
pred <- predictions(brms_numeric2, newdata = datagrid(hp = c(100, 120)))
expect_predictions2(brms_numeric2, newdata = datagrid(hp = c(100, 120)), se = FALSE)
expect_equal(dim(components(pred, "draws")), c(2, 2000))
# interaction
pred <- predictions(brms_interaction, newdata = datagrid(mpg = c(20, 25)))
expect_predictions2(brms_interaction, newdata = datagrid(mpg = c(20, 25)), se = FALSE)
# factor in data frame
pred <- predictions(brms_factor, newdata = datagrid())
expect_predictions2(brms_factor, newdata = datagrid(), se = FALSE)


# predictions: prediction vs. expectation vs. include_random
# prediction vs. response
p1 <- suppressWarnings(predictions(brms_epi, type = "prediction"))
p2 <- suppressWarnings(predictions(brms_epi, type = "response"))
expect_true(all(p1$conf.low < p2$conf.low))
expect_true(all(p1$conf.high > p2$conf.high))
# re_formula
p1 <- predictions(brms_epi, newdata = datagrid(patient = 1))
p2 <- predictions(brms_epi, newdata = datagrid(patient = 1), re_formula = NA)
expect_false(p1$estimate == p2$estimate)
expect_false(p1$conf.low == p2$conf.low)
expect_false(p1$conf.high == p2$conf.high)


# predictions vs. emmeans
testthat::skip_if_not_installed("emmeans")
requiet("emmeans")
em <- emmeans::emmeans(brms_numeric, ~hp, "hp", at = list(hp = c(100, 120)))
em <- data.frame(em)
pred <- predictions(brms_numeric, newdata = datagrid(hp = c(100, 120)), type = "link")
expect_equal(pred$estimate, em$emmean, ignore_attr = TRUE)
expect_equal(pred$conf.low, em$lower.HPD, ignore_attr = TRUE)
expect_equal(pred$conf.high, em$upper.HPD, ignore_attr = TRUE)


# marginaleffects: no validity
expect_slopes2(brms_numeric2, se = FALSE)
expect_slopes2(brms_interaction, se = FALSE)
expect_slopes2(brms_factor, se = FALSE)
# credible intervals and posterior draws
tmp <- slopes(brms_factor)
expect_true("conf.low" %in% colnames(tmp))
expect_true(all(tmp$estimate > tmp$conf.low))
expect_true(all(tmp$estimate < tmp$conf.high))
draws <- components(tmp, "draws")
expect_false(is.null(draws))
expect_equal(nrow(draws), nrow(tmp), ignore_attr = TRUE)


# marginaleffects vs. emmeans
testthat::skip_if_not_installed("emmeans")
requiet("emmeans")

# # known frequentist example to compare syntax
# brms_numeric_freq <- glm(am ~ hp, data = mtcars, family = binomial)
# slopes(brms_numeric_freq, newdata = datagrid(hp = 147), type = "link")
# emmeans::emtrends(brms_numeric_freq, specs = ~hp, var = "hp", at = list(hp = 147))

# one variable: link scale
mfx1 <- slopes(brms_numeric, variables = "hp", newdata = datagrid(hp = 110), type = "link")
mfx2 <- as.data.frame(emmeans::emtrends(brms_numeric, ~hp, var = "hp", at = list(hp = 110)))
expect_equal(mfx1$estimate, mfx2$hp.trend, ignore_attr = TRUE)
expect_equal(mfx1$conf.low, mfx2$lower.HPD, ignore_attr = TRUE)
expect_equal(mfx1$conf.high, mfx2$upper.HPD, ignore_attr = TRUE)

# one variable: response scale
mfx1 <- slopes(brms_numeric, variables = "hp", newdata = datagrid(hp = 110))
mfx2 <- as.data.frame(emtrends(brms_numeric, ~hp, var = "hp", at = list(hp = 110), regrid = "response"))
expect_equal(mfx1$estimate, mfx2$hp.trend, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001, ignore_attr = TRUE)

# numeric + factor: numeric variable
dat <- datagrid(model = brms_factor, mpg = 25, cyl_fac = 4)
mfx1 <- slopes(brms_factor, variables = "mpg", newdata = dat, type = "link")
mfx2 <- as.data.frame(emmeans::emtrends(brms_factor, ~mpg, var = "mpg", at = list(mpg = 25, cyl_fac = 4)))
expect_equal(mfx1$estimate, mfx2$mpg.trend, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001, ignore_attr = TRUE)

# numeric + factor: factor
dat <- datagrid(model = brms_factor, mpg = 25, cyl_fac = 4)
mfx1 <- slopes(brms_factor, variables = "cyl_fac", newdata = dat, type = "link")
mfx2 <- emmeans::emmeans(brms_factor, ~cyl_fac, var = "cyl_fac", at = list(mpg = 25))
mfx2 <- emmeans::contrast(mfx2, method = "revpairwise")
mfx2 <- data.frame(mfx2)[1:2, ]
expect_equal(mfx1$estimate, mfx2$estimate, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001, ignore_attr = TRUE)


# # plot_predictions: no validity
# p <- plot_predictions(brms_interaction, condition = c("mpg", "vs"))
# vdiffr::expect_doppelganger("brms logit plot_predictions", p)
#
#
#
# # plot_predictions: no validity
# p <- plot_predictions(brms_interaction, condition = c("mpg", "vs"))
# vdiffr::expect_doppelganger("brms logit plot_predictions", p)

# factor in formula
expect_error(slopes(brms_factor_formula), regexp = "factor")
expect_error(predictions(brms_factor_formula), regexp = "factor")


# bugs stay dead: factor indexing for posterior draws
tmp <- predictions(brms_factor, newdata = datagrid(cyl_fac = 4, mpg = c(10, 20)))
expect_s3_class(get_draws(tmp), "data.frame")


# mo() recognized as factor: Issue #220
# marginaleffects
mfx1 <- slopes(brms_monotonic)
mfx2 <- slopes(brms_monotonic, variable = "carb")
expect_error(slopes(brms_monotonic_factor), regexp = "cannot be used")
expect_s3_class(mfx1, "marginaleffects")
expect_s3_class(mfx2, "marginaleffects")

# comparisons
expect_error(comparisons(brms_monotonic_factor), regexp = "cannot be used")
contr1 <- avg_comparisons(brms_monotonic)
known <- c(sprintf("%s - 1", c(2:4, 6, 8)), "+1")
expect_equal(contr1$contrast, known, ignore_attr = TRUE)


# multivariate outcome
beta <- get_coef(brms_mv_1)
expect_equal(length(beta), 12, ignore_attr = TRUE)

mfx <- slopes(brms_mv_1)
expect_s3_class(mfx, "marginaleffects")

pred <- predictions(brms_mv_1)
expect_s3_class(pred, "predictions")

comp <- comparisons(brms_mv_1)
expect_s3_class(comp, "comparisons")

draws <- get_draws(mfx)
expect_s3_class(draws, "data.frame")
expect_true(all(c("drawid", "draw", "rowid") %in% colnames(draws)))

# categorical outcome
mfx <- slopes(brms_categorical_1)
expect_s3_class(mfx, "marginaleffects")

pred <- predictions(brms_categorical_1)
expect_s3_class(pred, "predictions")

comp <- comparisons(brms_categorical_1)
expect_s3_class(comp, "comparisons")

draws <- get_draws(mfx)
expect_s3_class(draws, "data.frame")
expect_true(all(c("drawid", "draw", "rowid") %in% colnames(draws)))


# vignette vdem example
p_response <- predictions(
    brms_vdem,
    type = "response",
    newdata = datagrid(
        party_autonomy = c(TRUE, FALSE),
        civil_liberties = .5,
        region = "Middle East and North Africa"
    )
)
expect_predictions2(brms_vdem, type = "response", newdata = datagrid(
    party_autonomy = c(TRUE, FALSE),
    civil_liberties = .5,
    region = "Middle East and North Africa"
), se = FALSE)
p_prediction <- predictions(
    brms_vdem,
    type = "prediction",
    newdata = datagrid(
        party_autonomy = c(TRUE, FALSE),
        civil_liberties = .5,
        region = "Middle East and North Africa"
    )
)
expect_predictions2(brms_vdem, type = "prediction", newdata = datagrid(
    party_autonomy = c(TRUE, FALSE),
    civil_liberties = .5,
    region = "Middle East and North Africa"
), se = FALSE)


# bugs stay dead: character regressors used to produce duplicates
expect_slopes2(brms_character, se = FALSE)
mfx <- avg_slopes(brms_character)
expect_true(length(unique(ti$estimate)) == nrow(ti))


# warning: vcov not supported
options(marginaleffects_safe = TRUE)
expect_warning(slopes(brms_numeric, vcov = "HC3"), regexp = "vcov.*not supported")
options(marginaleffects_safe = FALSE)

# Andrew Heiss says that lognormal_hurdle are tricky because the link is
# identity even if the response is actually logged
# https://github.com/vincentarelbundock/marginaleffects/issues/343

# non-hurdle part: post-calculation exponentiation
p1 <- predictions(
    brms_lognormal_hurdle,
    newdata = datagrid(lifeExp = seq(30, 80, 10)),
    transform = exp,
    dpar = "mu"
)
p2 <- predictions(
    brms_lognormal_hurdle,
    newdata = datagrid(lifeExp = seq(30, 80, 10)),
    dpar = "mu"
)
expect_true(all(p1$estimate != p2$estimate))

eps <- 0.01
cmp1 <- comparisons(
    brms_lognormal_hurdle,
    variables = list(lifeExp = eps),
    newdata = datagrid(lifeExp = seq(30, 80, 10)),
    comparison = function(hi, lo) (exp(hi) - exp(lo)) / exp(eps),
    dpar = "mu"
)
cmp2 <- comparisons(
    brms_lognormal_hurdle,
    variables = list(lifeExp = eps),
    newdata = datagrid(lifeExp = seq(30, 80, 10)),
    comparison = function(hi, lo) exp((hi - lo) / eps),
    dpar = "mu"
)
expect_true(all(cmp1$estimate != cmp2$estimate))

cmp <- comparisons(
    brms_lognormal_hurdle2,
    dpar = "mu",
    datagrid(disp = c(150, 300, 450)),
    comparison = "expdydx"
)

expect_equal(
    cmp$estimate,
    c(-0.0464610297239711, -0.0338017059188856, -0.0245881481374242),
    # seed difference?
    # c(-0.0483582312992919, -0.035158983842012, -0.0255763979591749),
    tolerance = .01
)

# emt <- emtrends(mod, ~disp, var = "disp", dpar = "mu",
#     regrid = "response", tran = "log", type = "response",
# at = list(disp = c(150, 300, 450)))

# Issue #432: bayes support for comparison with output of length 1
cmp1 <- comparisons(brms_numeric2, comparison = "difference")
cmp2 <- comparisons(brms_numeric2, comparison = "differenceavg")
cmp3 <- comparisons(brms_numeric2, comparison = "ratio")
cmp4 <- comparisons(brms_numeric2, comparison = "ratioavg")
expect_equal(nrow(cmp1), 64, ignore_attr = TRUE)
expect_equal(nrow(cmp2), 2, ignore_attr = TRUE)
expect_equal(nrow(cmp3), 64, ignore_attr = TRUE)
expect_equal(nrow(cmp4), 2, ignore_attr = TRUE)

# Issue #432: comparisons = conf.low = conf.high because mean() returns a
# single number when applied to the draws matrix
cmp <- comparisons(brms_binomial, variables = "tx", comparison = "lnoravg")
expect_true(all(cmp$estimate != cmp$conf.low))
expect_true(all(cmp$estimate != cmp$conf.high))
expect_true(all(cmp$conf.high != cmp$conf.low))

# Issue #432: get_draws() and tidy() error with `comparison="avg"`
pd <- get_draws(cmp)
expect_s3_class(pd, "data.frame")
expect_equal(nrow(pd), 4000, ignore_attr = TRUE)
ti <- tidy(cmp)
expect_equal(nrow(ti), 1, ignore_attr = TRUE)
expect_s3_class(ti, "data.frame")


# hypothesis with bayesian models
p1 <- predictions(
    brms_numeric2,
    hypothesis = c(1, -1),
    newdata = datagrid(hp = c(100, 110))
)

p2 <- predictions(
    brms_numeric2,
    hypothesis = "b1 = b2",
    newdata = datagrid(hp = c(100, 110))
)

expect_s3_class(p1, "predictions")
expect_s3_class(p2, "predictions")
expect_equal(nrow(p1), 1, ignore_attr = TRUE)
expect_equal(nrow(p2), 1, ignore_attr = TRUE)
expect_equal(p1$estimate, p2$estimate, ignore_attr = TRUE)
expect_true(all(c("conf.low", "conf.high") %in% colnames(p1)))
expect_true(all(c("conf.low", "conf.high") %in% colnames(p2)))

lc <- matrix(c(1, -1, -1, 1), ncol = 2)
colnames(lc) <- c("Contrast A", "Contrast B")
p3 <- predictions(
    brms_numeric2,
    hypothesis = lc,
    newdata = datagrid(hp = c(100, 110))
)
expect_s3_class(p3, "predictions")
expect_equal(nrow(p3), 2, ignore_attr = TRUE)
expect_equal(p3$term, c("Contrast A", "Contrast B"), ignore_attr = TRUE)
expect_equal(p3$estimate[1], -p3$estimate[2], ignore_attr = TRUE)


# `by` argument is supported for predictions() because it is a simple average.
# In comparisons(), some transformations are non-collapsible, so we can't just
# take the average, and we need to rely on more subtle transformations from
# `comparison_function_dict`.
p <- predictions(
    brms_factor,
    by = "cyl_fac"
)
expect_s3_class(p, "predictions")
expect_equal(ncol(components(p, "draws")), 2000)
expect_equal(nrow(p), 3, ignore_attr = TRUE)
expect_true(all(c("conf.low", "conf.high") %in% colnames(p)))


# `by` data frame to collapse response group
by <- data.frame(
    group = as.character(1:4),
    by = rep(c("(1,2)", "(3,4)"), each = 2)
)
p <- predictions(
    brms_cumulative_random,
    by = by
)
expect_equal(nrow(p), 2, ignore_attr = TRUE)
p <- predictions(
    brms_cumulative_random,
    by = by,
    hypothesis = ~reference
)
expect_equal(nrow(p), 1, ignore_attr = TRUE)


# # `by` not supported in comparisons() or slopes()
# # this is not supported!!
# expect_error(comparisons(brms_factor, by = "cyl_fac"), regexp = "supported")
# expect_error(slopes(brms_factor, by = "cyl_fac"), regexp = "supported")

# interaction is same order of magnitude as frequentist
# issue reported by Solomon Kurz over Twitter DM
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ am * factor(cyl), data = mtcars)

tid <- avg_comparisons(mod, variables = c("cyl", "am"), cross = TRUE)
tid.b <- avg_comparisons(brms_kurz, variables = c("cyl", "am"), cross = TRUE)


expect_equal(tid$estimate, tid.b$estimate, tolerance = 0.1, ignore_attr = TRUE)
expect_equal(tid$conf.low, tid.b$conf.low, tolerance = 0.2, ignore_attr = TRUE)
expect_equal(tid$conf.high, tid.b$conf.high, tolerance = 0.2, ignore_attr = TRUE)


# issue 445 leftover browser()
p <- predictions(mod, by = "am")
expect_s3_class(p, "predictions")
expect_equal(nrow(p), 2, ignore_attr = TRUE)


# transform works for comparisons() and predictions()
p1 <- predictions(brms_poisson, type = "link")
p2 <- predictions(brms_poisson, type = "link", transform = exp)
expect_equal(exp(p1$estimate), p2$estimate, ignore_attr = TRUE)
expect_equal(exp(p1$conf.low), p2$conf.low, ignore_attr = TRUE)
expect_equal(exp(p1$conf.high), p2$conf.high, ignore_attr = TRUE)
expect_equal(exp(components(p1, "draws")), components(p2, "draws"))

p1 <- comparisons(brms_poisson, type = "link")
p2 <- comparisons(brms_poisson, type = "link", transform = exp)
expect_equal(exp(p1$estimate), p2$estimate, ignore_attr = TRUE)
expect_equal(exp(p1$conf.low), p2$conf.low, ignore_attr = TRUE)
expect_equal(exp(p1$conf.high), p2$conf.high, ignore_attr = TRUE)
expect_equal(exp(components(p1, "draws")), components(p2, "draws"))


# byfun
by <- data.frame(
    by = c("1,2", "1,2", "3,4", "3,4"),
    group = 1:4
)
p1 <- predictions(brms_cumulative_random, newdata = "mean")
p2 <- predictions(brms_cumulative_random, newdata = "mean", by = by)
p3 <- predictions(brms_cumulative_random, newdata = "mean", by = by, byfun = sum)
expect_equal(mean(p1$estimate[1:2]), p2$estimate[1], tolerance = 0.1, ignore_attr = TRUE)
expect_equal(mean(p1$estimate[3:4]), p2$estimate[2], tolerance = 0.1, ignore_attr = TRUE)
expect_equal(sum(p1$estimate[1:2]), p3$estimate[1], tolerance = 0.1, ignore_attr = TRUE)
expect_equal(sum(p1$estimate[3:4]), p3$estimate[2], tolerance = 0.1, ignore_attr = TRUE)


# Issue #500
p <- plot_predictions(brms_issue500, condition = "z")
expect_s3_class(p, "gg")


# Issue #504: integrate out random effects
set.seed(1024)

K <<- 100

cmp <- avg_comparisons(
    brms_logit_re,
    newdata = datagrid(firm = sample(1e5:2e6, K)),
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian"
)

bm <- brmsmargins(
    k = K,
    object = brms_logit_re,
    at = data.frame(x = c(0, 1)),
    CI = .95,
    CIType = "ETI",
    contrasts = cbind("AME x" = c(-1, 1)),
    effects = "integrateoutRE"
)$ContrastSummary

expect_equal(cmp$estimate, bm$Mdn, tolerance = .05, ignore_attr = TRUE)
expect_equal(cmp$conf.low, bm$LL, tolerance = .05, ignore_attr = TRUE)
expect_equal(cmp$conf.high, bm$UL, tolerance = .05, ignore_attr = TRUE)


# get_draws(shape = )
tid <- avg_comparisons(brms_numeric2)
pd <- get_draws(tid, shape = "DxP")
hyp <- brms::hypothesis(pd, "hp - mpg > 0")
expect_s3_class(hyp, "brmshypothesis")


# posterior::rvar
tid <- avg_comparisons(brms_numeric2)
rv <- get_draws(tid, "rvar")
expect_equal(nrow(rv), 2, ignore_attr = TRUE)
expect_s3_class(rv$rvar[[1]], "rvar")


# Issue #546
cmp <- comparisons(brms_numeric2, newdata = datagrid())
expect_false(anyNA(cmp$am))


# Issue #576
cmp <- comparisons(brms_issue576)
expect_equal(nrow(cmp), 32, ignore_attr = TRUE)
cmp <- comparisons(brms_issue576, by = "term")
expect_equal(nrow(cmp), 1, ignore_attr = TRUE)
cmp <- comparisons(brms_issue576, by = "cyl")
expect_equal(nrow(cmp), 3, ignore_attr = TRUE)
cmp <- comparisons(brms_issue576, by = "am")
expect_equal(nrow(cmp), 2, ignore_attr = TRUE)


# Issue #639
pre <- avg_predictions(brms_issue639)
cmp <- avg_comparisons(brms_issue639)
expect_s3_class(pre, "predictions")
expect_s3_class(cmp, "comparisons")
expect_equal(nrow(pre), 5, ignore_attr = TRUE)
expect_equal(nrow(cmp), 5, ignore_attr = TRUE)


# Issue #703
pre <- predictions(brms_inhaler_cat, type = "link")
expect_s3_class(pre, "predictions")
cmp <- comparisons(brms_inhaler_cat, type = "link")
expect_s3_class(cmp, "comparisons")


# Issue #751: informative error on bad predition
expect_error(comparisons(brms_logit_re, newdata = datagrid(firm = -10:8)), regexp = "new.levels")
cmp = comparisons(brms_logit_re, newdata = datagrid(firm = -10:8), allow_new_levels = TRUE)
expect_s3_class(cmp, "comparisons")


# Issue #888: get_draws() fails for quantile transformation
expect_error(
    predictions(
        brms_factor,
        by = "cyl_fac",
        transform = \(x) ecdf(mtcars$mpg)(x)
    ) |>
        get_draws(),
    regexp = "matrix input must return"
)


# Issue 1006: predictor is also a response
cmp <- avg_comparisons(brms_issue1006)
expect_s3_class(cmp, "comparisons")
cmp <- avg_comparisons(brms_issue1006, variables = list(chl = 1))
expect_s3_class(cmp, "comparisons")
cmp <- avg_comparisons(brms_issue1006, variables = list(chl = 1))
expect_s3_class(cmp, "comparisons")
cmp <- avg_comparisons(brms_issue1006, variables = list(chl = 1, age = 1))
expect_s3_class(cmp, "comparisons")


# Issue #1313: data.table indexing in avg_comparisons()
set.seed(48103)
Cndtn <- factor(c("PC", "QR", "TK", "NG"))
TstTm <- c(0, 1)
data <- expand.grid(Cndtn = Cndtn, TstTm = TstTm)
data <- do.call(rbind, replicate(50, data, simplify = FALSE))
Age <- rnorm(200, 0, 2)
data$Age <- Age[rep(seq_len(200), each = 2)]
data$Y <- rnorm(400, 0, 0.5) +
    0.3 * (rnorm(400, 0, 0.2) + as.numeric(data$TstTm) - 1) +
    0.4 * (rnorm(400, 0, 0.2) + data$Age) +
    0.2 * ((rnorm(400, 0, 0.2) + as.numeric(data$TstTm) - 1)) * (rnorm(400, 0, 0.2) + as.numeric(data$Cndtn) / 10)
void <- capture.output(
    mdl <- brm(Y ~ TstTm + (TstTm:Cndtn) * Age, data = data, silent = 2)
)

by <- data.frame(
    Cndtn = c("PC", "PC", "QR", "QR", "TK", "TK", "NG", "NG"),
    TstTm = c("0", "1", "0", "1", "0", "1", "0", "1"),
    by = c("PC0", "PC1", "QR0", "QR1", "LG0", "LG1", "LG0", "LG1")
)
cmp1 <- avg_comparisons(
    mdl,
    datagrid(Cndtn = c("PC", "QR", "TK", "NG"), grid_type = "counterfactual"),
    variables = "Age",
    by = by
)
cmp2 <- avg_comparisons(
    mdl,
    datagrid(Cndtn = c("PC", "QR", "TK", "NG"), grid_type = "counterfactual"),
    variables = "Age",
    by = c("Cndtn", "TstTm")
)
expect_equal(
    subset(cmp1, by == "PC1")$estimate,
    subset(cmp2, Cndtn == "PC" & TstTm == 1)$estimate
)
expect_equal(
    subset(cmp1, by == "PC0")$estimate,
    subset(cmp2, Cndtn == "PC" & TstTm == 0)$estimate
)
expect_equal(
    subset(cmp1, by == "LG0")$estimate,
    subset(cmp2, Cndtn == "NG" & TstTm == 0)$estimate
)


# Issue #1508: cannot post-process with `hypotheses()`. Issue #1508.
cmp <- avg_comparisons(brms_factor, variables = list("cyl_fac" = "pairwise"))
expect_error(hypotheses(cmp), regexp = "posterior.*get_draws")


# Issue #1392: type="link" doesn't work
p <- predictions(brms_categorical_2_num, type = "link")
expect_false(anyNA(p$estimate))
expect_false(anyNA(p$group))
expect_false(anyNA(p$rowid))


# Issue #1615: binomial models with trials() syntax should plot with points
# Tests that plot_predictions() handles multiple response variables correctly
void <- capture.output({
    df_bioassay <- data.frame(
        dose = c(-0.86, -0.30, -0.05, 0.73),
        batch_size = c(5, 5, 5, 5),
        deaths = c(0, 1, 3, 5)
    )
    brms_binomial_trials <- brm(
        deaths | trials(batch_size) ~ dose,
        family = binomial(),
        data = df_bioassay,
        refresh = 0,
        silent = 2
    )
})
p <- plot_predictions(brms_binomial_trials, condition = "dose", points = 1)
vdiffr::expect_doppelganger("pkg-brms_issue1615", p)

