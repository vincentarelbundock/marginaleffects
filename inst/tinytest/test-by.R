source("helpers.R")
using("marginaleffects")

requiet("margins")
requiet("nnet")
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


# use comparison to collapse into averages
mod <- glm(gear ~ cyl + am, family = poisson, data = mtcars)
x <- tidy(comparisons(mod, comparison = "dydx"))
y <- comparisons(mod, comparison = "dydxavg")
expect_equivalent(x$estimate, y$estimate)
expect_equivalent(x$std.error, y$std.error)

x <- tidy(comparisons(mod, comparison = "eyex"))
y <- comparisons(mod, comparison = "eyexavg")
expect_equivalent(x$estimate, y$estimate)
expect_equivalent(x$std.error, y$std.error)

x <- tidy(comparisons(mod, comparison = "eydx"))
y <- comparisons(mod, comparison = "eydxavg")
expect_equivalent(x$estimate, y$estimate)
expect_equivalent(x$std.error, y$std.error)

x <- tidy(comparisons(mod, comparison = "dyex"))
y <- comparisons(mod, comparison = "dyexavg")
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
cmp <- comparisons(mod, by = "am", comparison = "lnor")
expect_equal(nrow(cmp), 4)

cmp <- comparisons(mod, by = "am")
tid <- tidy(cmp)

expect_equivalent(nrow(tid), nrow(cmp))
expect_equivalent(nrow(tid), 4)
expect_true("am" %in% colnames(tid))



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
    variables = "gear")
expect_equivalent(nrow(mm), 3)
expect_error(marginal_means(mod, by = TRUE, variables = "gear"))


# Issue #649: sort group when `by` is used
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
pre <- predictions(mod, by = "cyl")
expect_equivalent(pre$cyl, c(4, 6, 8))
cmp <- predictions(mod, by = "cyl")
expect_equivalent(cmp$cyl, c(4, 6, 8))


# marginaleffects poisson vs. margins
dat <- mtcars
mod <- glm(gear ~ cyl + am, family = poisson, data = dat)
mfx <- avg_slopes(
    mod,
    by = c("cyl", "am"),
    newdata = datagrid(
        cyl = unique,
        am = unique,
        grid_type = "counterfactual"))
mar <- margins(mod, at = list(cyl = unique(dat$cyl), am = unique(dat$am)))
mar <- summary(mar)
# margins doesn't treat the binary am as binary automatically
expect_equivalent(mfx$estimate[7:12], mar$AME[7:12], tolerance = tol)
expect_equivalent(mfx$std.error[7:12], mar$SE[7:12], tolerance = tol_se)


# comparisons poisson vs. margins
dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- glm(gear ~ cyl + am, family = poisson, data = dat)
mfx <- comparisons(
    mod,
    by = c("cyl", "am"),
    newdata = datagrid(
        cyl = unique,
        am = unique,
        grid_type = "counterfactual"))

mfx <- tidy(mfx)

mfx <- mfx[order(mfx$term, mfx$contrast, mfx$cyl, mfx$am),]
mar <- margins(mod, at = list(cyl = unique(dat$cyl), am = unique(dat$am)))
mar <- summary(mar)
expect_equivalent(mfx$estimate, mar$AME, tolerance = tol)
expect_equivalent(mfx$std.error, mar$SE, tolerance = tol_se)


# Issue #715: incorrect grouping with custom `comparison` function
dat <- transform(mtcars, vs = vs, am = as.factor(am), cyl = as.factor(cyl))
mod <- lm(mpg ~ qsec + am + cyl, dat)
fun <- \(hi, lo) mean(hi) / mean(lo)
cmp1 <- comparisons(mod, variables = "cyl", comparison = fun, by = "am")
cmp2 <- comparisons(mod, variables = "cyl", comparison = "ratioavg", by = "am")
expect_equivalent(cmp1$estimate, cmp2$estimate)
expect_equivalent(cmp1$std.error, cmp2$std.error)
expect_equal(nrow(cmp1), 4)


# https://stackoverflow.com/questions/75858227/in-rs-marginaleffects-package-why-do-these-two-methods-shows-different-results
requiet("dplyr")
tmp <- mtcars %>% transform(am = factor(am), cyl = factor(cyl), mpg = mpg)
mod <- lm(mpg ~ am * cyl, data = tmp)
cmp1 <- avg_comparisons(mod, variables = "am", by = "cyl")
cmp2 <- comparisons(mod, variables = "am") %>%
  group_by(cyl) %>%
  summarize(estimate = mean(estimate), .groups = "keep") |>
  ungroup()
cmp3 <- predictions(mod) |>
  group_by(am, cyl) |>
  summarize(estimate = mean(estimate), .groups = "keep") |>
  ungroup() |>
  group_by(cyl) |>
  summarize(estimate = diff(estimate), .groups = "keep") |>
  ungroup()
cmp4 <- transform(tmp, estimate = predict(mod))
cmp4 <- aggregate(estimate ~ cyl + am, FUN = mean, data = cmp4)
cmp4 <- aggregate(estimate ~ cyl, FUN = diff, data = cmp4)
expect_equivalent(cmp1$estimate, cmp2$estimate)
expect_equivalent(cmp1$estimate, cmp3$estimate)
expect_equivalent(cmp1$estimate, cmp4$estimate)




rm(list = ls())
