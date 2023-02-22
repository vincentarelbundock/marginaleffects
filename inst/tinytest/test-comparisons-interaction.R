source("helpers.R")
using("marginaleffects")

requiet("emmeans")

# interaction automatic flip from NULL to useful
dat <- mtcars
dat$gear <- factor(dat$gear)
dat$cyl <- factor(dat$cyl)
dat <- dat
mod1 <- lm(mpg ~ gear + cyl + wt + gear, data = dat)
mod2 <- lm(mpg ~ gear * cyl + wt + gear, data = dat)
cmp1 <- comparisons(mod1, newdata = datagrid())
cmp2 <- suppressWarnings(comparisons(mod2, newdata = datagrid(), cross = FALSE))
cmp3 <- suppressWarnings(comparisons(mod2, variables = c("cyl", "gear"), newdata = datagrid(), cross = TRUE))
expect_true("contrast" %in% colnames(cmp1))
expect_true("contrast" %in% colnames(cmp2))
expect_true(all(c("contrast_cyl", "contrast_gear") %in% colnames(cmp3)))

# variables must be unnamed vector
expect_error(comparisons(
    mod2,
    variables = c("cyl" = "ratio", "gear" = "difference"),
    newdata = datagrid()),
    pattern = "May not have names")

# interaction vs. emmeans
mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
cmp <- suppressWarnings(comparisons(
    mod,
    variables = list("cyl" = "all", "am" = "all"),
    newdata = datagrid(),
    cross = TRUE))
em <- emmeans(mod, c("cyl", "am"))
em <- emmeans::contrast(em, method = "revpairwise")
em <- data.frame(em)
expect_true(all(round(abs(em$estimate), 5) %in% round(abs(cmp$estimate), 5)))
expect_true(all(round(abs(em$SE), 5) %in% round(abs(cmp$std.error), 5)))


# cross and tidy()
mod <- lm(mpg ~ factor(cyl) + factor(am), data = mtcars)
cmp <- comparisons(mod, variables = c("am", "cyl"), cross = TRUE)
avg <- tidy(cmp)
expect_equivalent(nrow(avg), 2)
expect_true("contrast_am" %in% colnames(avg))
expect_true("contrast_cyl" %in% colnames(avg))


# tidy does not error (no validity)
mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
cmp <- comparisons(mod, variables = c("am", "cyl"), cross = TRUE)
tid <- tidy(cmp)
expect_true(all(tid$term == "cross"))



# `variables` must be specified
mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
cmp <- comparisons(mod, variables = c("am", "cyl"), cross = TRUE)
expect_inherits(cmp, "comparisons")
expect_error(comparisons(mod, cross = TRUE), pattern = "variables")



# interaction (no validity)
mod <- lm(mpg ~ factor(am) * factor(cyl) + wt + gear, data = mtcars)

# one row only means tidy is same nrows
# on some machines I get 21 rows instead of 18, but can't replicate. maybe look into this if I have the energy. Seems minor.
cmp <- comparisons(
    mod,
    variables = list("cyl" = "all", "am" = "all"),
    newdata = datagrid(),
    cross = TRUE)
expect_true(nrow(cmp) > 17) 
expect_true(nrow(tidy(cmp)) > 17)

cmp <- comparisons(
    mod,
    variables = list("cyl" = "sequential", "am" = "sequential"),
    cross = TRUE)
expect_equivalent(nrow(cmp), 64)
expect_equivalent(nrow(tidy(cmp)), 2)

cmp <- comparisons(
    mod,
    cross = TRUE,
    variables = c("cyl", "am", "wt"))
expect_equivalent(nrow(cmp), 64)
expect_equivalent(nrow(tidy(cmp)), 2)


# deprecated argument
expect_warning(comparisons(mod, interaction = TRUE))


# brms + order of first character doesn't matter
mod <- marginaleffects:::modelarchive_model("brms_factor")
cmp <- comparisons(mod, variables = list("cyl_fac" = "all", "mpg" = 1), cross = TRUE)
expect_equivalent(nrow(cmp), 192)
expect_equivalent(nrow(tidy(cmp)), 6)


rm(list = ls())