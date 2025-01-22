source("helpers.R")
using("marginaleffects")

requiet("emmeans")

dat <- mtcars
dat$carb <- factor(dat$carb)
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ carb + cyl, dat)



# hypothesis formula for model coefficients
mod <- lm(mpg ~ factor(cyl) + 0, data = mtcars)
h <- hypotheses(mod, hypothesis = ~reference)
expect_inherits(h, "hypotheses")


# informative errors and warnings
tmp <- lm(mpg ~ drat + wt, data = mtcars)
expect_error(slopes(tmp, hypothesis = "drat = wt"), pattern = "newdata")
expect_error(comparisons(tmp, hypothesis = "drat = wt"), pattern = "newdata")

expect_error(
    slopes(mod, newdata = dat, hypothesis = ~pairwise),
    pattern = "safe")

expect_warning(
    slopes(mod, lincom = "pairwise"),
    pattern = "lincom")

tmp <- lm(mpg ~ wt + drat, data = mtcars)
expect_error(
    predictions(
        tmp,
        hypothesis = "wt = drat",
        newdata = datagrid(wt = 2:3)),
    pattern = "unique row")


tmp <- mtcars
tmp$gear <- factor(tmp$gear)
expect_error(
    comparisons(
        lm(mpg ~ gear, tmp),
        newdata = "mean",
        variables = list(gear = "all"),
        hypothesis = "gear = 0"),
    pattern = "indices")

expect_error(
    slopes(
        mod,
        newdata = "mean",
        hypothesis = c(1, 1, 1),
        variables = "cyl"),
    pattern = "3 rows")

# errors
expect_error(
    slopes(
        mod,
        newdata = "mean",
        hypothesis = matrix(rep(1, 6), ncol = 2),
        variables = "cyl"),
    pattern = "2 rows")

# marginaleffects: hypothesis
mfx <- slopes(
    mod,
    newdata = "mean",
    variables = "cyl",
    hypothesis = ~pairwise)
expect_inherits(mfx, "marginaleffects")
expect_equivalent(nrow(mfx), 2)


# contrasts: hypothesis
cmp1 <- comparisons(
    mod,
    variables = "cyl",
    newdata = "mean")
cmp2 <- comparisons(
    mod,
    variables = "cyl",
    newdata = "mean",
    hypothesis = ~pairwise)
expect_equivalent(diff(cmp1$estimate), cmp2$estimate[1])


# marginaleffects: hypothesis
mfx <- slopes(
    mod,
    newdata = "mean",
    variables = "cyl",
    hypothesis = ~pairwise)
expect_inherits(mfx, "marginaleffects")
expect_equivalent(nrow(mfx), 2)


# predictions: hypothesis
p1 <- predictions(
    mod,
    newdata = datagrid(cyl = c(4, 6)),
    hypothesis = c(-1, 1))
p2 <- predictions(
    mod,
    datagrid(cyl = c(4, 6)))
expect_equivalent(p1$estimate, diff(p2$estimate))

lc <- matrix(c(
    -1, 1,
    -1, 0
), ncol = 2)
p3 <- predictions(
    mod,
    datagrid(cyl = c(4, 6)),
    hypothesis = lc)
expect_inherits(p3, "predictions")
expect_true(all(p3$term == "custom"))

# hypothesis matrix colnames become labels
colnames(lc) <- c("Contrast A", "Contrast B")
p3 <- predictions(
    mod,
    datagrid(cyl = c(4, 6)),
    hypothesis = lc)
expect_inherits(p3, "predictions")
expect_equivalent(p3$term, c("Contrast A", "Contrast B"))

# wildcard
mm1 <- suppressWarnings(predictions(mod, by = "cyl", hypothesis = "b* = b1"))
expect_equal(mm1$hypothesis, paste0("b", 1:3, "=b1"))
expect_equal(mm1$estimate[1], 0)


# marginaleffects: string function
mod <- lm(mpg ~ hp + drat, data = mtcars)
mfx1 <- slopes(
    mod,
    newdata = "mean",
    hypothesis = "exp(b1 + b2) = 100")
mfx2 <- slopes(
    mod,
    newdata = "mean",
    hypothesis = "exp(hp + drat) = 100")
expect_inherits(mfx1, "marginaleffects")
expect_equivalent(mfx1$estimate, mfx2$estimate)
expect_equivalent(mfx1$std.error, mfx2$std.error)


# predictions: string formulas
p1 <- predictions(
    mod,
    newdata = datagrid(hp = c(100, 110, 120)))
p2 <- predictions(
    mod,
    hypothesis = "b1 + b2 + b3 = 10",
    newdata = datagrid(hp = c(100, 110, 120)))
p3 <- predictions(
    mod,
    hypothesis = "b1 = b2",
    newdata = datagrid(hp = c(100, 110, 120)))
expect_equivalent(sum(p1$estimate) - 10, p2$estimate)
expect_equivalent(p1$estimate[1] - p1$estimate[2], p3$estimate)


# pad missing character levels + hypothesis
dat <- mtcars
dat$cyl <- as.character(dat$cyl)
mod <- lm(mpg ~ cyl, data = dat)
p <- predictions(
    mod,
    hypothesis = "b1 = b2",
    newdata = datagrid(cyl = c("6", "8")))
expect_inherits(p, "predictions")
expect_equivalent(nrow(p), 1)



# Issue #439
mod <- lm(mpg ~ factor(cyl) * factor(am), data = mtcars)
cmp <- comparisons(
    mod,
    variables = "am",
    by = "cyl",
    hypothesis = ~pairwise)
expect_inherits(cmp, "comparisons")
expect_equivalent(nrow(cmp), 6)

cmp <- comparisons(
    mod,
    variables = "am",
    by = "cyl",
    hypothesis = ~reference)
expect_inherits(cmp, "comparisons")
expect_equivalent(nrow(cmp), 2)


# Issue #559
mod <- lm(mpg ~ hp + drat, data = mtcars)
H <- matrix(c(0, 1, -1, 1 / 3, 1 / 3, 1 / 3), ncol = 2)
colnames(H) <- c("H1", "H2")
dm <- hypotheses(mod, hypothesis = H)
expect_equivalent(dm$term, c("H1", "H2"))


# Informative error on row mismatch
mod <- lm(mpg ~ hp + drat, data = mtcars)
expect_error(
    predictions(mod, newdata = "mean", hypothesis = "b1=b2"),
    pattern = "hypothesis testing")

# Issue #661: remove redundant labels in pairwise comparisons
if (!requiet("tinysnapshot")) exit_file("tinysnapshot")
using("tinysnapshot")
set.seed(123)
dat <- transform(iris, dummy = as.factor(rbinom(nrow(iris), 1, prob = c(0.4, 0.6))))
m <- lm(Sepal.Width ~ Sepal.Length * Species + dummy, data = dat)
mfx <- slopes(m, variables = "Sepal.Length", by = c("Species", "dummy"), hypothesis = ~pairwise)
expect_true("setosa, 0 - setosa, 1" %in% mfx$hypothesis)


# Issue #1092: hypothesis = "mean", "meanother"
mod <- lm(mpg ~ hp + drat + factor(cyl), data = mtcars)
p1 <- avg_predictions(mod, by = "cyl")
p2 <- avg_predictions(mod, by = "cyl", hypothesis = ~meandev)
expect_equivalent(p2$estimate, p1$estimate - mean(p1$estimate))
p2 <- avg_predictions(mod, by = "cyl", hypothesis = ~meanotherdev)
expect_equivalent(p2$estimate, p1$estimate - (sum(p1$estimate) - p1$estimate) / (nrow(p1) - 1))

# Issue #1345: no names
dat <- transform(mtcars, carb = factor(carb))
mod <- glm(am ~ carb + mpg, family = binomial("logit"), data = dat)
custom_contrast <<- function(x) {
    w <- contr.poly(6)[, 1:2] # weights
    out <- setNames(
        as.vector(x %*% w),
        nm = c("linear", "quadratic")
    )
    names(out) <- NULL
    out
}
p <- predictions(mod,
    variables = list("carb" = levels, "mpg" = "sd"),
    hypothesis = ~ I(custom_contrast(x)) | rowidcf + mpg,
    type = "response")
expect_inherits(p, "predictions")
expect_false("hypothesis" %in% colnames(p))
expect_true("mpg" %in% colnames(p))
expect_true("rowidcf" %in% colnames(p))


# Issue #1345: names
dat <- transform(mtcars, carb = factor(carb))
mod <- glm(am ~ carb + mpg, family = binomial("logit"), data = dat)
custom_contrast <<- function(x) {
    w <- contr.poly(6)[, 1:2] # weights
    out <- setNames(
        as.vector(x %*% w),
        nm = c("linear", "quadratic")
    )
    out
}
p <- predictions(mod,
    variables = list("carb" = levels, "mpg" = "sd"),
    hypothesis = ~ I(custom_contrast(x)) | rowidcf + mpg,
    type = "response")
expect_inherits(p, "predictions")
expect_true("hypothesis" %in% colnames(p))
expect_true("mpg" %in% colnames(p))
expect_true("rowidcf" %in% colnames(p))


# Issue #1345: bayesian
requiet("brms")
requiet("rstan")
brms_factor <- readRDS("modelarchive/data/brms_factor.rds")
p <- avg_predictions(brms_factor, by = "cyl_fac", hypothesis = ~reference)
d <- get_draws(p)
expect_inherits(p, "predictions")
expect_true("hypothesis" %in% colnames(p))
expect_true(all(c("6 - 4", "8 - 4") %in% d$hypothesis))

p <- predictions(brms_factor,
    hypothesis = ~ I(c(a = x[1], b = mean(x[1:2]))) | cyl_fac)
d <- get_draws(p)
expect_inherits(p, "predictions")
expect_true("hypothesis" %in% colnames(p))
expect_true(all(c("a", "b") %in% p$hypothesis))
expect_true(all(c("a", "b") %in% d$hypothesis))

p <- predictions(brms_factor,
    hypothesis = ~ I(mean(x)) | cyl_fac)
expect_inherits(p, "predictions")
expect_false("hypothesis" %in% colnames(p))
expect_true("cyl_fac" %in% colnames(p))


# Issue #1349
names_diff <<- \(x) setNames(diff(x), paste0("diff-", 2:length(x)))
mod <- lm(mpg ~ am * factor(cyl), data = mtcars)
p <- predictions(mod)
h <- hypotheses(p, hypothesis = ~ I(names_diff(x)) | am)
h <- hypotheses(h, hypothesis = ~ I(mean(x)) | term) |> suppressWarnings()
expect_false(anyNA(h$estimate))


dat = data.table::data.table(iris)
dat[, big := as.numeric(Sepal.Width > mean(Sepal.Width))]
dat = dat[order(Species, big)]
mod <- lm(Sepal.Length ~ big * Species * Petal.Length, data = dat)

cmp <- avg_comparisons(mod,
    variables = "big",
    hypothesis = ~ reference | Species,
    by = c("big", "Species"))
expect_inherits(cmp, "comparisons")

cmp <- avg_comparisons(mod,
    variables = "Petal.Length",
    hypothesis = ~ meandev | Species,
    by = c("big", "Species"))
expect_inherits(cmp, "comparisons")

pre <- avg_predictions(mod,
    hypothesis = ratio ~ sequential | big,
    by = c("big", "Species"))
expect_inherits(pre, "predictions")



rm(list = ls())
