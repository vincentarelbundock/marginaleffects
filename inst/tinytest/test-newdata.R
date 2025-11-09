source("helpers.R")
using("marginaleffects")

requiet("modelbased")
requiet("emmeans")
requiet("dplyr")
requiet("MatchIt")


# # this seems deprecated in modelbased in favor of get_datagrid(). Have not investidated yet
# # visualisation_matrix() without `x` variable
# mod <- lm(mpg ~ hp + factor(cyl), mtcars)

# p1 <- predictions(mod, newdata = datagrid(cyl = mtcars$cyl))
# p2 <- predictions(mod, newdata = visualisation_matrix(at = "cyl"))
# expect_equivalent(nrow(p1), nrow(p2))
# expect_true(all(c("newdata_adjusted_for", "newdata_at_specs") %in% names(attributes(p2))))

# m1 <- slopes(mod, newdata = datagrid(cyl = mtcars$cyl))
# m2 <- slopes(mod, newdata = visualisation_matrix(at = "cyl"))
# expect_equivalent(nrow(m1), nrow(m2))
# expect_true(all(c("newdata_adjusted_for", "newdata_at_specs") %in% names(attributes(m2))))

# shortcut labels
dat <- mtcars
mod <- glm(vs ~ hp + factor(cyl), family = binomial, data = dat)
cmp1 <- comparisons(mod, newdata = "mean")
cmp2 <- comparisons(mod, newdata = "median")
expect_true(all(cmp1$hp == round(mean(dat$hp))))
expect_true(all(cmp2$hp == stats::median(dat$hp)))
expect_true(all(cmp2$estimate != cmp1$estimate))


# newdata = 'marginalmeans'
dat <- mtcars
dat$gear <- factor(dat$gear)
dat$cyl <- factor(dat$cyl)
dat$am <- factor(dat$am)
mod <- lm(mpg ~ gear + cyl + am, data = dat)
cmp <- avg_comparisons(mod, newdata = "balanced", variables = "gear", by = "gear") |>
    subset(gear == 3)

emm <- emmeans(mod, specs = "gear")
emm <- data.frame(emmeans::contrast(emm, method = "trt.vs.ctrl1"))

expect_equivalent(cmp$estimate, emm$estimate, tolerance = 1e-6)
expect_equivalent(cmp$std.error, emm$SE, tolerance = 1e-6)


# TODO: works interactively but not in tinytest
# # Issue #624: reserved "group" word in `by` and `newdata` but not in model.
# dat <- transform(mtcars, group = cyl)
# mod <- lm(mpg ~ hp, data = dat)
# expect_error(slopes(mod, newdata = dat, by = "group"), pattern = "forbidden")
# expect_inherits(slopes(mod, newdata = dat, by = "cyl"), "slopes")

# Issue #814
data(lalonde, package = "MatchIt")
if (exists("mdata")) rm(mdata)
test <- function() {
    mdata <- lalonde
    m0 <- lm(re78 ~ nodegree, data = mdata)
    comparisons(m0, variables = "nodegree", newdata = subset(mdata, nodegree == 1))
}
cmp1 <- test()
mdata <- subset(lalonde, married == 1)
m0 <- lm(re78 ~ nodegree, data = mdata)
cmp2 <- comparisons(m0, variables = "nodegree", newdata = subset(mdata, nodegree == 1))
cmp3 <- test()
expect_equal(nrow(cmp1), nrow(subset(lalonde, nodegree == 1)))
expect_equal(nrow(cmp2), nrow(subset(lalonde, nodegree == 1 & married == 1)))
expect_equal(nrow(cmp3), nrow(subset(lalonde, nodegree == 1)))


# Issue 1045: subset in newdata
data("lalonde", package = "MatchIt")
fit <- lm(re78 ~ treat * (age + educ), data = lalonde)
k = avg_comparisons(fit, variables = "treat", newdata = subset(lalonde, treat == 1))$estimate
x = avg_comparisons(fit, variables = "treat", newdata = base::subset(treat == 1))$estimate
y = avg_comparisons(fit, variables = "treat", newdata = filter(treat == 1))$estimate
w = avg_comparisons(fit, variables = "treat", newdata = dplyr::filter(treat == 1))$estimate
expect_equal(k, x)
expect_equal(k, y)
expect_equal(k, w)
