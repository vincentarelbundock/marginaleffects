source("helpers.R")
using("marginaleffects")

requiet("emmeans")

dat <- mtcars
dat$carb <- factor(dat$carb)
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ carb + cyl, dat)




# informative errors and warnings
tmp <- lm(mpg ~ drat + wt, data = mtcars)
expect_error(slopes(tmp, hypothesis = "drat = wt"), pattern = "newdata")
expect_error(comparisons(tmp, hypothesis = "drat = wt"), pattern = "newdata")

expect_error(
    slopes(mod, newdata = dat, hypothesis = "pairwise"),
    pattern = "smaller")

expect_warning(
    slopes(mod, lincom = "pairwise"),
    pattern = "lincom")

tmp <- lm(mpg ~ wt + drat, data = mtcars)
expect_error(predictions(
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
    slopes(mod, newdata = dat, hypothesis = "reference"),
    pattern = "smaller")

expect_error(slopes(
    mod,
    newdata = "mean",
    hypothesis = c(1, 1, 1),
    variables = "cyl"),
    pattern = "of length")

# errors
expect_error(slopes(
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
    hypothesis = "pairwise")
expect_inherits(mfx, "marginaleffects")
expect_equivalent(nrow(mfx), 1)


# contrasts: hypothesis
cmp1 <- comparisons(
    mod,
    variables = "cyl",
    newdata = "mean")
cmp2 <- comparisons(
    mod,
    variables = "cyl",
    newdata = "mean",
    hypothesis = "revpairwise")
expect_equivalent(diff(cmp1$estimate), cmp2$estimate)


# marginaleffects: hypothesis
mfx <- slopes(
    mod,
    newdata = "mean",
    variables = "cyl",
    hypothesis = "pairwise")
expect_inherits(mfx, "marginaleffects")
expect_equivalent(nrow(mfx), 1)


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

# marginalmeans: hypothesis complex
lc <- c(-2, 1, 1, 0, -1, 1)
em <- emmeans(mod, "carb") 
em <- emmeans::contrast(em, method = data.frame(custom_contrast = lc))
em <- data.frame(em)
mm <- marginal_means(mod, variables = "carb", hypothesis = lc)
expect_equivalent(mm$estimate, em$estimate)
expect_equivalent(mm$std.error, em$SE)

# marginalmeans: hypothesis shortcut
mm <- marginal_means(mod, variables = "carb", hypothesis = "reference")
expect_equivalent(nrow(mm), 5)
mm <- marginal_means(mod, variables = "carb", hypothesis = "sequential")
expect_equivalent(nrow(mm), 5)
mm <- marginal_means(mod, variables = "carb", hypothesis = "pairwise")
expect_equivalent(nrow(mm), 15)

# marginalmeans: hypothesis complex matrix
lc <- matrix(c(
    -2, 1, 1, 0, -1, 1,
    -1, 1, 0, 0, 0, 0
    ), ncol = 2)
mm <- marginal_means(mod, variables = "carb", hypothesis = lc)
expect_inherits(mm, "marginalmeans")
expect_equal(nrow(mm), 2)


# marginalmeans: string function
mm1 <- marginal_means(
    mod,
    hypothesis = "b1 + b2 = 12")
mm2 <- marginal_means(mod)
expect_equivalent(
    mm2$estimate[1] + mm2$estimate[2] - 12,
    mm1$estimate)


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
    hypothesis = "pairwise")
expect_inherits(cmp, "comparisons")
expect_equivalent(nrow(cmp), 3)

cmp <- comparisons(
    mod,
    variables = "am",
    by = "cyl",
    hypothesis = "reference")
expect_inherits(cmp, "comparisons")
expect_equivalent(nrow(cmp), 2)


# Issue #559
mod <- lm(mpg ~ hp + drat, data = mtcars)
H <- matrix(c(0, 1, -1, 1/3, 1/3, 1/3), ncol = 2)
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
mfx <- slopes(m, variables = "Sepal.Length", by = c("Species", "dummy"), hypothesis = "pairwise")
expect_true("setosa, 0 - setosa, 1" %in% mfx$term)


# # Issue #568
# # TODO: p-value computed before transform; null on the pre-transform scale
# mod <- glm(vs ~ hp, data = mtcars, family = binomial)

# comparisons(mod,
#     newdata = "mean",
#     comparison = "ratio")

# comparisons(mod,
#     newdata = "mean",
#     comparison = "ratio",
#     hypothesis = 0)

# comparisons(mod,
#     newdata = "mean",
#     comparison = "ratio",
#     hypothesis = 1)

# marginaleffects

# hypotheses(mod, hypothesis = -2)

# predictions(mod, newdata = "mean", hypothesis = .75)

# slopes(mod, newdata = "mean", hypothesis = .75)


rm(list = ls())