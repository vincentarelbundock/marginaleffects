source("helpers.R")
using("marginaleffects")

exit_if_not(requiet("margins"))
exit_if_not(requiet("nnet"))
tol <- 1e-4
tol_se <- 1e-3


mod1 <- glm(gear ~ cyl + am, family = poisson, data = mtcars)
mod2 <- lm(gear ~ cyl + am, data = mtcars)
p1 <- predictions(mod1, by = "am")
p2 <- predictions(mod2, by = "am")
p3 <- predictions(mod2, by = "am", wts = mtcars$wt)
expect_true("conf.low" %in% colnames(p1))
expect_true("conf.low" %in% colnames(p2))
expect_equivalent(nrow(p1), nrow(p2))
expect_equivalent(nrow(p1), 2)


# use transform_pre to collapse into averages
mod <- glm(gear ~ cyl + am, family = poisson, data = mtcars)
x <- tidy(comparisons(mod, transform_pre = "dydx"))
y <- comparisons(mod, transform_pre = "dydxavg")
expect_equivalent(x$estimate, y$estimate)
expect_equivalent(x$std.error, y$std.error)

x <- tidy(comparisons(mod, transform_pre = "eyex"))
y <- comparisons(mod, transform_pre = "eyexavg")
expect_equivalent(x$estimate, y$estimate)
expect_equivalent(x$std.error, y$std.error)

x <- tidy(comparisons(mod, transform_pre = "eydx"))
y <- comparisons(mod, transform_pre = "eydxavg")
expect_equivalent(x$estimate, y$estimate)
expect_equivalent(x$std.error, y$std.error)

x <- tidy(comparisons(mod, transform_pre = "dyex"))
y <- comparisons(mod, transform_pre = "dyexavg")
expect_equivalent(x$estimate, y$estimate)
expect_equivalent(x$std.error, y$std.error)

x <- tidy(slopes(mod, slope = "dyex"))
y <- slopes(mod, slope = "dyexavg")
expect_equivalent(x$estimate, y$estimate)
expect_equivalent(x$std.error, y$std.error)

# input sanity check
expect_error(slopes(mod, slope = "bad"), pattern = "eyexavg")

##### aggregate() refactor makes this possible again
# by is deprecated in `summary()` and `tidy()`
# expect_error(summary(comparisons(mod), by = "am"), pattern = "instead")
# expect_error(tidy(comparisons(mod), by = "am"), pattern = "instead")

# by argument
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
cmp <- comparisons(mod, by = "am", transform_pre = "lnor")
expect_equal(nrow(cmp), 4)

cmp <- comparisons(mod, by = "am")
tid <- tidy(cmp)

expect_equivalent(nrow(tid), nrow(cmp))
expect_equivalent(nrow(tid), 4)
expect_true("am" %in% colnames(tid))


# marginaleffects poisson vs. margins
dat <- mtcars
mod <- glm(gear ~ cyl + am, family = poisson, data = dat)
mfx <- slopes(
    mod,
    by = c("cyl", "am"),
    newdata = datagrid(
        cyl = dat$cyl,
        am = dat$am,
        grid_type = "counterfactual"))
tid <- tidy(mfx)
tid <- tid[order(tid$term, tid$cyl, tid$am),]
mar <- margins(mod, at = list(cyl = unique(dat$cyl), am = unique(dat$am)))
mar <- summary(mar)
expect_equivalent(tid$estimate, mar$AME, tolerance = tol)
expect_equivalent(tid$std.error, mar$SE, tolerance = tol_se)


# comparisons poisson vs. margins
dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- glm(gear ~ cyl + am, family = poisson, data = dat)
mfx <- comparisons(
    mod,
    by = c("cyl", "am"),
    newdata = datagrid(
        cyl = dat$cyl,
        am = dat$am,
        grid_type = "counterfactual"))

mfx <- tidy(mfx)

mfx <- mfx[order(mfx$term, mfx$contrast, mfx$cyl, mfx$am),]
mar <- margins(mod, at = list(cyl = unique(dat$cyl), am = unique(dat$am)))
mar <- summary(mar)
expect_equivalent(mfx$estimate, mar$AME, tolerance = tol)
expect_equivalent(mfx$std.error, mar$SE, tolerance = tol_se)


# # input checks
# mod <- lm(mpg ~ hp, mtcars)
# expect_error(comparisons(mod, by = "am"), pattern = "newdata")
# expect_error(slopes(mod, by = "am"), pattern = "newdata")


# counterfactual margins at()
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ factor(cyl) * hp + wt, data = dat)
mar <- margins(mod, at = list(cyl = unique(dat$cyl)))
mar <- data.frame(summary(mar))
mfx <- slopes(
    mod,
    by = "cyl",
    newdata = datagridcf(cyl = c(4, 6, 8)))
expect_equivalent(mfx$estimate, mar$AME)
expect_equivalent(mfx$std.error, mar$SE, tolerance = 1e6)



# issue #434 by with character precitors
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/AER/Affairs.csv")
mod <- glm(
    affairs ~ children + gender + yearsmarried,
    family = poisson,
    data = dat)
p <- predictions(mod, by = "children")
expect_equivalent(nrow(p), 2)
expect_false(anyNA(p$estimate))


# Issue #445: by data frame to collapse response levels
mod <- nnet::multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)

expect_error(predictions(mod, type = "probs", by = "response"), pattern = "Character vector")
expect_error(predictions(mod, type = "probs", by = mtcars), pattern = "Character vector")

p <- predictions(mod, type = "probs", by = "group")
expect_equivalent(nrow(p), 3)
cmp <- comparisons(mod, type = "probs", by = "group")
expect_equivalent(nrow(cmp), 9)

by <- data.frame(
    group = c("3", "4", "5"),
    by = c("(3,4)", "(3,4)", "(5)"))
p1 <- predictions(mod, type = "probs")
p2 <- predictions(mod, type = "probs", by = by)
p3 <- predictions(mod, type = "probs", by = by, hypothesis = "sequential")
p4 <- predictions(mod, type = "probs", by = by, hypothesis = "reference")
p5 <- predictions(mod, type = "probs", by = c("am", "vs", "group"))
expect_equivalent(mean(subset(p1, group == "5")$estimate), p2$estimate[2])
expect_equivalent(p3$estimate, diff(p2$estimate))
expect_equivalent(nrow(p4), 1)
expect_equivalent(nrow(p5), 12)

cmp <- comparisons(mod, type = "probs", by = "am")
expect_equivalent(nrow(cmp), 18)

cmp <- comparisons(
    mod,
    variables = "am",
    by = by,
    type = "probs")
expect_equivalent(nrow(cmp), 2)

cmp <- comparisons(
    mod,
    variables = "am",
    by = by,
    hypothesis = "sequential",
    type = "probs")
expect_equivalent(nrow(cmp), 1)


# Issue #481: warning on missing by categories
mod <- nnet::multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
by <- data.frame(
    by = c("4", "5"),
    group = 4:5)
expect_warning(comparisons(mod, variables = "mpg", newdata = "mean", by = by))
expect_warning(predictions(mod, newdata = "mean", by = by))


# Issue #589: easy marginalization
mod <- lm(mpg ~ factor(gear) + am, mtcars)
cmp1 <- comparisons(mod, by = TRUE)
cmp2 <- comparisons(mod, by = FALSE)
expect_equivalent(nrow(cmp1), 3)
expect_equivalent(nrow(cmp2), 96)

pre1 <- predictions(mod, by = TRUE)
pre2 <- predictions(mod, by = FALSE)
expect_equivalent(nrow(pre1), 1)
expect_equivalent(nrow(pre2), 32)

pre1 <- slopes(mod, by = TRUE)
pre2 <- slopes(mod, by = FALSE)
expect_equivalent(nrow(pre1), 3)
expect_equivalent(nrow(pre2), 96)

mm <- marginal_means(
    mod,
    by = FALSE,
    variables = "gear")
expect_equivalent(nrow(mm), 3)
expect_error(marginal_means(mod, by = TRUE, variables = "gear"))