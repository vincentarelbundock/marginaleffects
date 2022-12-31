source("helpers.R", local = TRUE)
using("tinyviztest")
exit_if_not(!ON_OSX)

requiet("dplyr")

dat <<- mtcars
mod <- glm(am ~ hp, data = dat, family = binomial)
cmp <- comparisons(mod, transform_pre = "lnor")
expect_error(summary(cmp), pattern = "collapsible")
expect_error(tidy(cmp), pattern = "collapsible")
cmp <- comparisons(mod, transform_pre = "lnoravg")
expect_equivalent(nrow(cmp), 1)


# simple summary output
mod <- lm(mpg ~ hp + factor(cyl), dat)
mfx <- marginaleffects(mod)
expect_pdiff(summary(mfx), "summary-marginaleffects")


# summary conf.level
mod <- lm(mpg ~ hp + factor(cyl), dat)
mfx <- marginaleffects(mod)
expect_pdiff(summary(mfx, conf_level = .9), "summary-marginaleffects_conf_level_90")
expect_pdiff(summary(mfx, conf_level = .2), "summary-marginaleffects_conf_level_20")


# summary: marginal means
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$vs <- as.logical(dat$vs)
dat$gear <- as.factor(dat$gear)
dat <<- dat
mod <- lm(mpg ~ gear + am + vs, dat)
mm <- marginalmeans(mod)
expect_pdiff(summary(mm), "summary-marginalmeans")


# bugs stay dead: summary manipulation (destroys attributes, unfortunately)
dat <<- mtcars
mod <- glm(am ~ hp * wt, data = dat, family = binomial)
mfx <- marginaleffects(mod)
expect_pdiff(
    summary(mfx) %>% dplyr::select(term, estimate, conf.low, conf.high),
    "summary-marginaleffects_dplyr")


# bugs stay dead: label transformation_post
dat <<- mtcars
mod <- glm(am ~ hp, data = dat, family = binomial)
cmp <- comparisons(mod, transform_pre = function(hi, lo) hi / lo, transform_post = exp)
expect_pdiff(summary(cmp), "summary-comparisons_transform_post")

