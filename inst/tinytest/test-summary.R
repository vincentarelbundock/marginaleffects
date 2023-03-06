source("helpers.R")
using("marginaleffects")
if (!requiet("tinysnapshot")) exit_file("tinysnapshot")
using("tinysnapshot")

requiet("poorman")

dat <- mtcars
mod <- glm(am ~ hp, data = dat, family = binomial)
cmp <- comparisons(mod, comparison = "lnor")

cmp <- comparisons(mod, comparison = "lnoravg")
expect_equivalent(nrow(cmp), 1)

# these two errors are no longer relevant, since we no longer aggregate in
# tidy(), but instead run the same call through comparisons(), which does fancy
# averaging by changing comparison when possible/known.
# expect_error(summary(cmp), pattern = "collapsible")
# expect_error(tidy(cmp), pattern = "collapsible")

# simple summary output
mod <- lm(mpg ~ hp + factor(cyl), dat)
mfx <- slopes(mod)
expect_snapshot_print(summary(mfx), "summary-marginaleffects")


# summary conf.level
mod <- lm(mpg ~ hp + factor(cyl), dat)
mfx <- slopes(mod)
expect_snapshot_print(summary(mfx, conf_level = .9), "summary-marginaleffects_conf_level_90")
expect_snapshot_print(summary(mfx, conf_level = .2), "summary-marginaleffects_conf_level_20")


# summary.hypotheses
mod <- lm(mpg ~ hp + factor(cyl), mtcars)
hyp <- hypotheses(mod, "b3 = b4")
expect_snapshot_print(summary(hyp), "summary-hypotheses")


# summary: marginal means
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$vs <- as.logical(dat$vs)
dat$gear <- as.factor(dat$gear)
dat <- dat
mod <- lm(mpg ~ gear + am + vs, dat)
mm <- marginal_means(mod)
expect_snapshot_print(summary(mm), "summary-marginalmeans")


# bugs stay dead: summary manipulation (destroys attributes, unfortunately)
dat <- mtcars
mod <- glm(am ~ hp * wt, data = dat, family = binomial)
mfx <- slopes(mod)
expect_snapshot_print(
    summary(mfx) |> poorman::select(term, estimate, conf.low, conf.high),
    "summary-marginaleffects_dplyr")


# bugs stay dead: label transformation_post
dat <- mtcars
mod <- glm(am ~ hp, data = dat, family = binomial)
cmp <- avg_comparisons(mod, comparison = function(hi, lo) hi / lo, transform = exp)
expect_snapshot_print(cmp, "summary-comparisons_transform")



rm(list = ls())