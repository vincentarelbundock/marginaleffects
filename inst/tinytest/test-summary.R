source("helpers.R")
exit_if_not(require("tinyviztest"))
exit_if_not(!ON_OSX)

using("tinyviztest")
using("marginaleffects")

exit_if_not(requiet("dplyr"))

dat <- mtcars
mod <- glm(am ~ hp, data = dat, family = binomial)
cmp <- comparisons(mod, transform_pre = "lnor")

cmp <- comparisons(mod, transform_pre = "lnoravg")
expect_equivalent(nrow(cmp), 1)

# these two errors are no longer relevant, since we no longer aggregate in
# tidy(), but instead run the same call through comparisons(), which does fancy
# averaging by changing transform_pre when possible/known.
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
expect_snapshot_print(summary(hyp, conf_level = .8), "summary-hypotheses_conf_level_80")


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
    summary(mfx) |> dplyr::select(term, estimate, conf.low, conf.high),
    "summary-marginaleffects_dplyr")


# bugs stay dead: label transformation_post
dat <- mtcars
mod <- glm(am ~ hp, data = dat, family = binomial)
cmp <- comparisons(mod, transform_pre = function(hi, lo) hi / lo, transform_post = exp)
expect_snapshot_print(summary(cmp), "summary-comparisons_transform_post")