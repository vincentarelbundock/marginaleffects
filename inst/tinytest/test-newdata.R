source("helpers.R")
using("marginaleffects")

requiet("modelbased")
requiet("emmeans")


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
expect_true(all(cmp1$hp == mean(dat$hp)))
expect_true(all(cmp2$hp == stats::median(dat$hp)))
expect_true(all(cmp2$estimate != cmp1$estimate))



# newdata = 'marginalmeans'
dat <- mtcars
dat$gear <- factor(dat$gear)
dat$cyl <- factor(dat$cyl)
dat$am <- factor(dat$am)
mod <- lm(mpg ~ gear + cyl + am, data = dat)
cmp <- comparisons(mod, newdata = "marginalmeans", variables = "gear")
cmp <- tidy(cmp)

emm <- emmeans(mod, specs = "gear")
emm <- data.frame(emmeans::contrast(emm, method = "trt.vs.ctrl1"))

expect_equivalent(cmp$estimate, emm$estimate)
expect_equivalent(cmp$std.error, emm$SE)



# Issue #624: reserved "group" word in `by` and `newdata` but not in model.
dat <- transform(mtcars, group = cyl)
mod <- lm(mpg ~ hp, data = dat)
expect_error(slopes(mod, newdata = dat, by = "group"), pattern = "forbidden")
expect_inherits(slopes(mod, newdata = dat, by = "cyl"), "slopes")



# the results are numerically correct, but it's a pain to get the exact same
# rows as emmeans

# # cross contrast: newdata = 'marginalmeans'
# dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
# mod <- lm(bill_length_mm ~ species * sex + island + body_mass_g, data = dat)

# cmp <- comparisons(
#     mod,
#     cross = TRUE,
#     newdata = "marginalmeans",
#     variables = list(species = "pairwise", island = "pairwise"))

# emm <- emmeans(mod, specs = c("species", "island"))
# emm <- data.frame(emmeans::contrast(emm, method = "trt.vs.ctrl1"))

# # hack: not sure if they are well aligned
# expect_equivalent(sort(cmp$estimate), sort(emm$estimate))
# expect_equivalent(sort(cmp$std.error), sort(emm$SE))




rm(list = ls())