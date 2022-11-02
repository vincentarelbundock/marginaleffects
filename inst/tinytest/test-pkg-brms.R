# HPD tests against emmeans, which uses HDI, but our default is ETI
# HDI is implemented specifically for these tests
# https://github.com/vincentarelbundock/marginaleffects/issues/240
source("helpers.R", local = TRUE)
# exit_file("expensive")
# if (ON_CI) exit_file("on ci")
if (ON_WINDOWS) exit_file("on windows")
if (ON_CRAN) exit_file("on cran")
if (!minver("base", "4.1.0")) exit_file("R 4.1.0")
options("marginaleffects_credible_interval" = "hdi")
requiet("brms")
requiet("emmeans")
requiet("broom")
requiet("brmsmargins")
tol <- 0.0001
tol_se <- 0.001


# download models
brms_numeric <- marginaleffects:::modelarchive_model("brms_numeric")
brms_numeric2 <- marginaleffects:::modelarchive_model("brms_numeric2")
brms_character <- marginaleffects:::modelarchive_model("brms_character")
brms_factor <- marginaleffects:::modelarchive_model("brms_factor")
brms_factor_formula <- marginaleffects:::modelarchive_model("brms_factor_formula")
brms_interaction <- marginaleffects:::modelarchive_model("brms_interaction")
brms_logical <- marginaleffects:::modelarchive_model("brms_logical")
brms_epi <- marginaleffects:::modelarchive_model("brms_epi")
brms_cumulative_random <- marginaleffects:::modelarchive_model("brms_cumulative_random")
brms_monotonic <- marginaleffects:::modelarchive_model("brms_monotonic")
brms_monotonic_factor <- marginaleffects:::modelarchive_model("brms_monotonic_factor")
brms_vdem <- marginaleffects:::modelarchive_model("brms_vdem")
brms_lognormal_hurdle <- marginaleffects:::modelarchive_model("brms_lognormal_hurdle")
brms_lognormal_hurdle2 <- marginaleffects:::modelarchive_model("brms_lognormal_hurdle2")
brms_binomial <- marginaleffects:::modelarchive_model("brms_binomial")
brms_mixed_3 <- insight::download_model("brms_mixed_3")
brms_mv_1 <- marginaleffects:::modelarchive_model("brms_mv_1")
brms_vdem <- marginaleffects:::modelarchive_model("brms_vdem")
brms_ordinal_1 <- insight::download_model("brms_ordinal_1")
brms_categorical_1 <- marginaleffects:::modelarchive_model("brms_categorical_1")
brms_logit_re <- marginaleffects:::modelarchive_model("brms_logit_re")



# average marginal effects brmsmargins
options("marginaleffects_credible_interval" = "eti")
h <- 5e-5
bm <- brmsmargins(
  brms_numeric,
  add = data.frame(hp = c(0, 0 + h)),
  contrasts = cbind("AME MPG" = c(-1 / h, 1 / h)),
  CI = 0.95, CIType = "ETI")
bm <- data.frame(bm$ContrastSummary)

mfx <- marginaleffects(brms_numeric)
mfx <- tidy(mfx)

expect_equivalent(mean(posteriordraws(mfx)$draw), bm$M, tolerance = tol)
expect_equivalent(mfx$conf.low, bm$LL, tolerance = tol)
expect_equivalent(mfx$conf.high, bm$UL, tolerance = tol)

options("marginaleffects_credible_interval" = "hdi")


# marginaleffects vs. emmeans
mfx <- marginaleffects(
    brms_numeric2,
    newdata = datagrid(mpg = 20, hp = 100),
    variables = "mpg",
    type = "link")

em <- emtrends(brms_numeric2, ~mpg, "mpg", at = list(mpg = 20, hp = 100))
em <- tidy(em)
expect_equivalent(mfx$dydx, em$mpg.trend)
expect_equivalent(mfx$conf.low, em$lower.HPD)
expect_equivalent(mfx$conf.high, em$upper.HPD)
# tolerance is less good for back-transformed response
mfx <- marginaleffects(brms_numeric2, newdata = datagrid(mpg = 20, hp = 100),
                   variables = "mpg", type = "response")
em <- emtrends(brms_numeric2, ~mpg, "mpg", at = list(mpg = 20, hp = 100), regrid = "response")
em <- tidy(em)
expect_equivalent(mfx$dydx, em$mpg.trend, tolerance = .1)
expect_equivalent(mfx$conf.low, em$lower.HPD, tolerance = .01)
expect_equivalent(mfx$conf.high, em$upper.HPD, tolerance = .1)


# brms: cumulative: marginaleffects: no validity
expect_marginaleffects(brms_cumulative_random, se = FALSE)


# brms: logical regressor
mfx <- marginaleffects(brms_logical)
expect_inherits(mfx, "marginaleffects")
expect_equivalent(nrow(mfx), nrow(attr(mfx, "posterior_draws")))


# predictions: hypothetical group
nd <- suppressWarnings(datagrid(model = brms_mixed_3, grp = 4, subgrp = 12))
nd$Subject <- 1000
set.seed(1024)
p1 <- predictions(brms_mixed_3, newdata = nd, allow_new_levels = TRUE)
set.seed(1024)
p2 <- predictions(brms_mixed_3, newdata = nd, allow_new_levels = TRUE, sample_new_levels = "gaussian")
set.seed(1024)
p3 <- predictions(brms_mixed_3, newdata = nd, allow_new_levels = TRUE, sample_new_levels = "uncertainty")
expect_false(any(p1$predicted == p2$predicted))
expect_equivalent(p1, p3)
expect_inherits(posteriordraws(p3), "data.frame")


# predictions w/ random effects

# link
w <- apply(posterior_linpred(brms_mixed_3), 2, stats::median)
x <- get_predict(brms_mixed_3, type = "link")
y <- predictions(brms_mixed_3, type = "link")
expect_equivalent(w, x$predicted)
expect_equivalent(w, y$predicted)

# response
w <- apply(posterior_epred(brms_mixed_3), 2, stats::median)
x <- get_predict(brms_mixed_3, type = "response")
y <- predictions(brms_mixed_3, type = "response")
expect_equivalent(w, x$predicted)
expect_equivalent(w, y$predicted)

# no random effects
w1 <- apply(posterior_epred(brms_mixed_3), 2, stats::median)
w2 <- apply(posterior_epred(brms_mixed_3, re_formula = NA), 2, stats::median)
x <- get_predict(brms_mixed_3, re_formula = NA, type = "response")
y <- predictions(brms_mixed_3, re_formula = NA, type = "response")
expect_true(all(w1 != w2))
expect_equivalent(w2, x$predicted)
expect_equivalent(w2, y$predicted)


# brms: cumulative: predictions: no validity
set.seed(1024)
p1 <- predictions(brms_cumulative_random)
p2 <- predictions(brms_cumulative_random, re_formula = NA)
expect_true(mean(p1$conf.low < p2$conf.low) > .95) # tolerance
expect_true(mean(p1$conf.high > p2$conf.high) > .99) # tolerance
expect_warning(predictions(brms_cumulative_random, include_random = FALSE)) # only for lme4


# marginaleffects: ordinal no validity
expect_marginaleffects(brms_ordinal_1, se = FALSE)


# predict new unit: no validity
dat1 <- dat2 <- datagrid(model = brms_epi)
dat2$patient <- 9999
set.seed(1024)
mfx1 <- marginaleffects(brms_epi, newdata = dat1)
set.seed(1024)
mfx2 <- marginaleffects(brms_epi, newdata = dat2, allow_new_levels = TRUE)
expect_false(any(mfx1$dydx == mfx2$dydx))



# tidy()
dat <- mtcars
dat$logic <- as.logical(dat$vs)
dat$cyl_fac <- as.factor(dat$cyl)
dat$cyl_cha <- as.character(dat$cyl)
mfx <- marginaleffects(brms_factor, newdata = dat)
ti <- tidy(mfx)
expect_inherits(ti, "data.frame")
expect_equivalent(dim(ti), c(3, 6))
expect_true(all(c("term", "estimate", "conf.low") %in% colnames(ti)))



# predictions: no validity
# simple
pred <- predictions(brms_numeric2, newdata = datagrid(hp = c(100, 120)))
expect_predictions(pred, se = FALSE)
expect_equivalent(dim(attr(pred, "posterior_draws")), c(2, 2000))
# interaction
pred <- predictions(brms_interaction, newdata = datagrid(mpg = c(20, 25)))
expect_predictions(pred, se = FALSE)
# factor in data frame
pred <- predictions(brms_factor, newdata = datagrid())
expect_predictions(pred, se = FALSE)



# predictions: prediction vs. expectation vs. include_random
# prediction vs. response
p1 <- suppressWarnings(predictions(brms_epi, type = "prediction"))
p2 <- suppressWarnings(predictions(brms_epi, type = "response"))
expect_true(all(p1$conf.low < p2$conf.low))
expect_true(all(p1$conf.high > p2$conf.high))
# re_formula
p1 <- predictions(brms_epi, newdata = datagrid(patient = 1))
p2 <- predictions(brms_epi, newdata = datagrid(patient = 1), re_formula = NA)
expect_false(p1$predicted == p2$predicted)
expect_false(p1$conf.low == p2$conf.low)
expect_false(p1$conf.high == p2$conf.high)



# predictions vs. emmeans
requiet("emmeans")
em <- emmeans::emmeans(brms_numeric, ~hp, "hp", at = list(hp = c(100, 120)))
em <- data.frame(em)
pred <- predictions(brms_numeric, newdata = datagrid(hp = c(100, 120)), type = "link")
expect_equivalent(pred$predicted, em$emmean)
expect_equivalent(pred$conf.low, em$lower.HPD)
expect_equivalent(pred$conf.high, em$upper.HPD)



# marginalmeans vs. emmeans
requiet("emmeans")
requiet("broom")
expect_error(marginalmeans(brms_factor, variables = "cyl_fac", type = "link"), pattern = "github.*issues")
# emmeans::emmeans(brms_factor, specs = ~cyl_fac)



# marginaleffects: no validity
expect_marginaleffects(brms_numeric2, se = FALSE)
expect_marginaleffects(brms_interaction, se = FALSE)
expect_marginaleffects(brms_factor, se = FALSE)
# credible intervals and posterior draws
tmp <- marginaleffects(brms_factor)
expect_true("conf.low" %in% colnames(tmp))
expect_true(all(tmp$dydx > tmp$conf.low))
expect_true(all(tmp$dydx < tmp$conf.high))
expect_false(is.null(attr(tmp, "posterior_draws")))
expect_equivalent(nrow(attr(tmp, "posterior_draws")), nrow(tmp))



# marginaleffects vs. emmeans
requiet("emmeans")

# # known frequentist example to compare syntax
# brms_numeric_freq <- glm(am ~ hp, data = mtcars, family = binomial)
# marginaleffects(brms_numeric_freq, newdata = datagrid(hp = 147), type = "link")
# emmeans::emtrends(brms_numeric_freq, specs = ~hp, var = "hp", at = list(hp = 147))

# one variable: link scale
mfx1 <- marginaleffects(brms_numeric, variables = "hp", newdata = datagrid(hp = 110), type = "link")
mfx2 <- as.data.frame(emmeans::emtrends(brms_numeric, ~hp, var = "hp", at = list(hp = 110)))
expect_equivalent(mfx1$dydx, mfx2$hp.trend)
expect_equivalent(mfx1$conf.low, mfx2$lower.HPD)
expect_equivalent(mfx1$conf.high, mfx2$upper.HPD)

# one variable: response scale
mfx1 <- marginaleffects(brms_numeric, variables = "hp", newdata = datagrid(hp = 110))
mfx2 <- as.data.frame(emtrends(brms_numeric, ~hp, var = "hp", at = list(hp = 110), regrid = "response"))
expect_equivalent(mfx1$dydx, mfx2$hp.trend, tolerance = .001)
expect_equivalent(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
expect_equivalent(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)

# numeric + factor: numeric variable
dat <- datagrid(model = brms_factor, mpg = 25, cyl_fac = 4)
mfx1 <- marginaleffects(brms_factor, variables = "mpg", newdata = dat, type = "link")
mfx2 <- as.data.frame(emmeans::emtrends(brms_factor, ~mpg, var = "mpg", at = list(mpg = 25, cyl_fac = 4)))
expect_equivalent(mfx1$dydx, mfx2$mpg.trend, tolerance = .001)
expect_equivalent(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
expect_equivalent(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)

# numeric + factor: factor
dat <- datagrid(model = brms_factor, mpg = 25, cyl_fac = 4)
mfx1 <- marginaleffects(brms_factor, variables = "cyl_fac", newdata = dat, type = "link")
mfx2 <- emmeans::emmeans(brms_factor, ~ cyl_fac, var = "cyl_fac", at = list(mpg = 25))
mfx2 <- emmeans::contrast(mfx2, method = "revpairwise")
mfx2 <- data.frame(mfx2)[1:2,]
expect_equivalent(mfx1$dydx, mfx2$estimate, tolerance = .001)
expect_equivalent(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
expect_equivalent(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)



# # plot_cap: no validity
# p <- plot_cap(brms_interaction, condition = c("mpg", "vs"))
# vdiffr::expect_doppelganger("brms logit plot_cap", p)
#
#
#
# # plot_cap: no validity
# p <- plot_cap(brms_interaction, condition = c("mpg", "vs"))
# vdiffr::expect_doppelganger("brms logit plot_cap", p)


# factor in formula
expect_error(marginaleffects(brms_factor_formula),
         pattern = "factor")
expect_error(predictions(brms_factor_formula),
         pattern = "factor")



# bugs stay dead: factor indexing for posterior draws
tmp <- predictions(brms_factor, newdata = datagrid(cyl_fac = 4, mpg = c(10, 20)))
expect_inherits(posteriordraws(tmp), "data.frame")



# mo() recognized as factor: Issue #220
# marginaleffects
mfx1 <- marginaleffects(brms_monotonic)
mfx2 <- marginaleffects(brms_monotonic, variable = "carb")
expect_error(marginaleffects(brms_monotonic_factor), pattern = "cannot be used")
expect_inherits(mfx1, "marginaleffects")
expect_inherits(mfx2, "marginaleffects")

# comparisons
expect_error(comparisons(brms_monotonic_factor), pattern = "cannot be used")
contr1 <- tidy(comparisons(brms_monotonic))
expect_true(all(paste(c(2, 3, 4, 6, 8), "-", 1) %in% contr1$contrast))
contr2 <- tidy(comparisons(brms_monotonic, contrast_factor = "pairwise", variables = "carb"))
expect_equivalent(nrow(contr2), 15)



# multivariate outcome
beta <- get_coef(brms_mv_1)
expect_equivalent(length(beta), 12)

mfx <- marginaleffects(brms_mv_1)
expect_inherits(mfx, "marginaleffects")

pred <- predictions(brms_mv_1)
expect_inherits(pred, "predictions")

comp <- comparisons(brms_mv_1)
expect_inherits(comp, "comparisons")

draws <- posteriordraws(mfx)
expect_inherits(draws, "data.frame")
expect_true(all(c("drawid", "draw", "rowid") %in% colnames(draws)))

# categorical outcome
mfx <- marginaleffects(brms_categorical_1)
expect_inherits(mfx, "marginaleffects")

pred <- predictions(brms_categorical_1)
expect_inherits(pred, "predictions")

comp <- comparisons(brms_categorical_1)
expect_inherits(comp, "comparisons")

draws <- posteriordraws(mfx)
expect_inherits(draws, "data.frame")
expect_true(all(c("drawid", "draw", "rowid") %in% colnames(draws)))


# vignette vdem example
p_response <- predictions(
    brms_vdem,
    type = "response",
    newdata = datagrid(
        party_autonomy = c(TRUE, FALSE),
        civil_liberties = .5,
        region = "Middle East and North Africa"))
expect_predictions(p_response, se = FALSE)
p_prediction <- predictions(
    brms_vdem,
    type = "prediction",
    newdata = datagrid(
        party_autonomy = c(TRUE, FALSE),
        civil_liberties = .5,
        region = "Middle East and North Africa"))
expect_predictions(p_prediction, se = FALSE)



# bugs stay dead: character regressors used to produce duplicates
expect_marginaleffects(brms_character, se = FALSE)
mfx <- marginaleffects(brms_character)
ti <- tidy(mfx)
expect_true(length(unique(ti$estimate)) == nrow(ti))



# warning: vcov not supported
expect_warning(marginaleffects(brms_numeric, vcov = "HC3"),
           pattern = "vcov.*not supported")

# Andrew Heiss says that lognormal_hurdle are tricky because the link is
# identity even if the response is actually logged
# https://github.com/vincentarelbundock/marginaleffects/issues/343

# non-hurdle part: post-calculation exponentiation
p1 <- predictions(
    brms_lognormal_hurdle,
    newdata = datagrid(lifeExp = seq(30, 80, 10)),
    transform_post = exp,
    dpar = "mu")
p2 <- predictions(
    brms_lognormal_hurdle,
    newdata = datagrid(lifeExp = seq(30, 80, 10)),
    dpar = "mu")
expect_true(all(p1$predicted != p2$predicted))

eps <- 0.01
cmp1 <- comparisons(
    brms_lognormal_hurdle,
    variables = list(lifeExp = eps),
    newdata = datagrid(lifeExp = seq(30, 80, 10)),
    transform_pre = function(hi, lo) (exp(hi) - exp(lo)) / exp(eps),
    dpar = "mu")
cmp2 <- comparisons(
    brms_lognormal_hurdle,
    variables = list(lifeExp = eps),
    newdata = datagrid(lifeExp = seq(30, 80, 10)),
    transform_pre = function(hi, lo) exp((hi - lo) / eps),
    dpar = "mu")
expect_true(all(cmp1$comparison != cmp2$comparison))

cmp <- comparisons(
    brms_lognormal_hurdle2,
    dpar = "mu",
    datagrid(disp = c(150, 300, 450)),
    transform_pre = "expdydx")

expect_equivalent(cmp$comparison, 
    c(-0.0464610297239711, -0.0338017059188856, -0.0245881481374242),
    # seed difference?
    # c(-0.0483582312992919, -0.035158983842012, -0.0255763979591749),
    tolerance = .01)

# emt <- emtrends(mod, ~disp, var = "disp", dpar = "mu", 
#     regrid = "response", tran = "log", type = "response",
    # at = list(disp = c(150, 300, 450)))

# Issue #432: bayes support for transform_pre with output of length 1
cmp1 <- comparisons(brms_numeric2, transform_pre = "difference")
cmp2 <- comparisons(brms_numeric2, transform_pre = "differenceavg")
cmp3 <- comparisons(brms_numeric2, transform_pre = "ratio")
cmp4 <- comparisons(brms_numeric2, transform_pre = "ratioavg")
expect_equivalent(nrow(cmp1), 64)
expect_equivalent(nrow(cmp2), 2)
expect_equivalent(nrow(cmp3), 64)
expect_equivalent(nrow(cmp4), 2)

# Issue #432: comparisons = conf.low = conf.high because mean() returns a
# single number when applied to the draws matrix
cmp <- comparisons(brms_binomial, variables = "tx", transform_pre = "lnoravg")
expect_true(all(cmp$comparison != cmp$conf.low))
expect_true(all(cmp$comparison != cmp$conf.high))
expect_true(all(cmp$conf.high != cmp$conf.low))

# Issue #432: posteriordraws() and tidy() error with `transform_pre="avg"`
pd <- posteriordraws(cmp)
expect_inherits(pd, "data.frame")
expect_equivalent(nrow(pd), 4000)
ti <- tidy(cmp)
expect_equivalent(nrow(ti), 1)
expect_inherits(ti, "data.frame")


# hypothesis with bayesian models
p1 <- predictions(
    brms_numeric2,
    hypothesis = c(1, -1),
    newdata = datagrid(hp = c(100, 110)))

p2 <- predictions(
    brms_numeric2,
    hypothesis = "b1 = b2",
    newdata = datagrid(hp = c(100, 110)))

expect_inherits(p1, "predictions")
expect_inherits(p2, "predictions")
expect_equivalent(nrow(p1), 1)
expect_equivalent(nrow(p2), 1)
expect_equivalent(p1$predicted, p2$predicted)
expect_true(all(c("conf.low", "conf.high") %in% colnames(p1)))
expect_true(all(c("conf.low", "conf.high") %in% colnames(p2)))

lc <- matrix(c(1, -1, -1, 1), ncol = 2)
colnames(lc) <- c("Contrast A", "Contrast B")
p3 <- predictions(
    brms_numeric2,
    hypothesis = lc,
    newdata = datagrid(hp = c(100, 110)))
expect_inherits(p3, "predictions")
expect_equivalent(nrow(p3), 2)
expect_equivalent(p3$term, c("Contrast A", "Contrast B"))
expect_equivalent(p3$predicted[1], -p3$predicted[2])


# `by` argument is supported for predictions() because it is a simple average.
# In comparisons(), some transformations are non-collapsible, so we can't just
# take the average, and we need to rely on more subtle transformations from
# `transform_pre_function_dict`.
p <- predictions(
    brms_factor,
    by = "cyl_fac")
expect_inherits(p, "predictions")
expect_equal(ncol(attr(p, "posterior_draws")), 2000)
expect_equal(nrow(p), 3)
expect_true(all(c("conf.low", "conf.high") %in% colnames(p)))


# `by` data frame to collapse response group
by <- data.frame(
    group = as.character(1:4),
    by = rep(c("(1,2)", "(3,4)"), each = 2))
p <- predictions(
    brms_cumulative_random,
    by = by)
expect_equivalent(nrow(p), 2)
p <- predictions(
    brms_cumulative_random,
    by = by,
    hypothesis = "reference")
expect_equivalent(nrow(p), 1)



# `by` not supported in comparisons() or marginaleffects()
expect_error(comparisons(brms_factor, by = "cyl_fac"), pattern = "supported")
expect_error(marginaleffects(brms_factor, by = "cyl_fac"), pattern = "supported")




# interaction is same order of magnitude as frequentist
# issue reported by Solomon Kurz over Twitter DM
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ am * factor(cyl), data = mtcars)
void <- capture.output(suppressMessages(
    mod.b <- brm(
        mpg ~ am * cyl,
        data = dat,
        family = gaussian,
        seed = 1024)
))

cmp <- comparisons(mod, variables = c("cyl", "am"), cross = TRUE)
cmp.b <- comparisons(mod.b, variables = c("cyl", "am"), cross = TRUE)
tid <- tidy(cmp)
tid.b <- tidy(cmp.b)


expect_equivalent(tid$estimate, tid.b$estimate, tolerance = 0.1)
expect_equivalent(tid$conf.low, tid.b$conf.low, tolerance = 0.2)
expect_equivalent(tid$conf.high, tid.b$conf.high, tolerance = 0.2)


# issue 445 leftover browser()
p <- predictions(mod, by = "am")
expect_inherits(p, "predictions")
expect_equivalent(nrow(p), 2)


# transform_post works for comparisons() and predictions()
void <- capture.output(suppressMessages(
    mod <- brm(gear ~ mpg + hp, data = mtcars, family = poisson)
))

p1 <- predictions(mod, type = "link")
p2 <- predictions(mod, type = "link", transform_post = exp)
expect_equivalent(exp(p1$predicted), p2$predicted)
expect_equivalent(exp(p1$conf.low), p2$conf.low)
expect_equivalent(exp(p1$conf.high), p2$conf.high)
expect_equivalent(exp(attr(p1, "posterior_draws")), attr(p2, "posterior_draws"))

p1 <- comparisons(mod, type = "link")
p2 <- comparisons(mod, type = "link", transform_post = exp)
expect_equivalent(exp(p1$comparison), p2$comparison)
expect_equivalent(exp(p1$conf.low), p2$conf.low)
expect_equivalent(exp(p1$conf.high), p2$conf.high)
expect_equivalent(exp(attr(p1, "posterior_draws")), attr(p2, "posterior_draws"))


# byfun
by <- data.frame(
    by = c("1,2", "1,2", "3,4", "3,4"),
    group = 1:4)
p1 <- predictions(brms_cumulative_random, newdata = "mean")
p2 <- predictions(brms_cumulative_random, newdata = "mean", by = by)
p3 <- predictions(brms_cumulative_random, newdata = "mean", by = by, byfun = sum)
expect_equivalent(mean(p1$predicted[1:2]), p2$predicted[1], tolerance = 0.1)
expect_equivalent(mean(p1$predicted[3:4]), p2$predicted[2], tolerance = 0.1)
expect_equivalent(sum(p1$predicted[1:2]), p3$predicted[1], tolerance = 0.1)
expect_equivalent(sum(p1$predicted[3:4]), p3$predicted[2], tolerance = 0.1)



# Issue #500
N <- 1250
n <- sample(10:100, size = N, replace = T)
x <- rbinom(N, 1, 0.5)
w <- rbinom(N, 1, 0.5)
z <- rbinom(N, 1, 0.5)
y <- rbinom(N, n, 0.25 + .25 * x + .125 * w + 0.05 * z)
d <- data.frame(x, w, z, y, n)
void <- capture.output(suppressMessages(
    fit <- brm(
        y | trials(n) ~ .,
        data = d,
        family = binomial(),
        chains = 1,
        verbose = FALSE)
))
p <- plot_cap(fit, condition = "z")
expect_inherits(p, "gg")


# Issue #504: integrate out random effects
set.seed(1024)

K <- 100

cmp <- comparisons(
    brms_logit_re,
    newdata = datagrid(firm = sample(1e5:2e6, K)),
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian") |>
    tidy()

bm <- brmsmargins(
  k = K,
  object = brms_logit_re,
  at = data.frame(x = c(0, 1)),
  CI = .95, CIType = "ETI",
  contrasts = cbind("AME x" = c(-1, 1)),
  effects = "integrateoutRE")$ContrastSummary

expect_equivalent(cmp$estimate, bm$Mdn, tolerance = .05)
expect_equivalent(cmp$conf.low, bm$LL, tolerance = .05)
expect_equivalent(cmp$conf.high, bm$UL, tolerance = .05)
