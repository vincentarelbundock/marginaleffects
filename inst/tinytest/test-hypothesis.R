source("helpers.R")
requiet("emmeans")

dat <- mtcars
dat$carb <- factor(dat$carb)
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ carb + cyl, dat)


# informative errors and warnings
tmp <- lm(mpg ~ drat + wt, data = mtcars)
expect_error(marginaleffects(tmp, hypothesis = "drat = wt"), pattern = "newdata")
expect_error(comparisons(tmp, hypothesis = "drat = wt"), pattern = "newdata")

expect_error(
    marginaleffects(mod, newdata = dat, hypothesis = "pairwise"),
    pattern = "smaller")

expect_warning(
    marginaleffects(mod, lincom = "pairwise"),
    pattern = "lincom")

tmp <- lm(mpg ~ wt + drat, data = mtcars)
expect_warning(predictions(
    tmp,
    newdata = datagrid(wt = 2:3),
    hypothesis = "wt = drat"),
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
    marginaleffects(mod, newdata = dat, hypothesis = "reference"),
    pattern = "smaller")

expect_error(marginaleffects(
    mod,
    newdata = "mean",
    hypothesis = c(1, 1, 1),
    variables = "cyl"),
    pattern = "of length")

# errors
expect_error(marginaleffects(
    mod,
    newdata = "mean",
    hypothesis = matrix(rep(1, 6), ncol = 2),
    variables = "cyl"),
    pattern = "2 rows")

# marginaleffects: hypothesis
mfx <- marginaleffects(
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
    hypothesis = "pairwise")
expect_equivalent(diff(cmp1$comparison), cmp2$comparison)


# marginaleffects: hypothesis
mfx <- marginaleffects(
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
expect_equivalent(p1$predicted, diff(p2$predicted))

lc <- matrix(c(
    -1, 1,
    -1, 0
    ), ncol = 2)
p3 <- predictions(
    mod,
    datagrid(cyl = c(4, 6)),
    hypothesis = lc)
expect_inherits(p3, "predictions")


# marginalmeans: hypothesis complex
lc <- c(-2, 1, 1, 0, -1, 1)
em <- emmeans(mod, "carb") 
em <- contrast(em, method = data.frame(custom_contrast = lc))
em <- data.frame(em)
mm <- marginalmeans(mod, variables = "carb", hypothesis = lc)
expect_equivalent(mm$marginalmean, em$estimate)
expect_equivalent(mm$std.error, em$SE)

# marginalmeans: hypothesis shortcut
mm <- marginalmeans(mod, variables = "carb", hypothesis = "reference")
expect_equivalent(nrow(mm), 5)

# marginalmeans: hypothesis complex matrix
lc <- matrix(c(
    -2, 1, 1, 0, -1, 1,
    -1, 1, 0, 0, 0, 0
    ), ncol = 2)
mm <- marginalmeans(mod, variables = "carb", hypothesis = lc)
expect_inherits(mm, "marginalmeans")
expect_equal(nrow(mm), 2)


# marginalmeans: string function
mm1 <- marginalmeans(
    mod,
    hypothesis = "b1 + b2 = 12")
mm2 <- marginalmeans(mod)
expect_equivalent(
    mm2$marginalmean[1] + mm2$marginalmean[2] - 12,
    mm1$marginalmean)


# marginaleffects: string function
mod <- lm(mpg ~ hp + drat, data = mtcars)
mfx1 <- marginaleffects(
    mod,
    newdata = "mean",
    hypothesis = "exp(b1 + b2) = 100")
mfx2 <- marginaleffects(
    mod,
    newdata = "mean",
    hypothesis = "exp(hp + drat) = 100")
expect_inherits(mfx1, "marginaleffects")
expect_equivalent(mfx1$dydx, mfx2$dydx)
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
expect_equivalent(sum(p1$predicted) - 10, p2$predicted)
expect_equivalent(p1$predicted[1] - p1$predicted[2], p3$predicted)


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
