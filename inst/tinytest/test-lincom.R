source("helpers.R")
requiet("emmeans")

dat <- mtcars
dat$carb <- factor(dat$carb)
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ carb + cyl, dat)


# errors
expect_error(
    marginaleffects(mod, lincom = "pairwise"),
    pattern = "smaller")

tmp <- mtcars
tmp$gear <- factor(tmp$gear)
expect_error(
    comparisons(
    lm(mpg ~ gear, tmp),
    newdata = "mean",
    variables = list(gear = "all"),
    lincom = "gear = 0"),
    pattern = "duplicate term names")

expect_error(
    marginaleffects(mod, lincom = "reference"),
    pattern = "smaller")

expect_false(expect_error(
    marginalmeans(mod, lincom = "pairwise"),
    pattern = "smaller"))

expect_error(marginaleffects(
    mod,
    newdata = "mean",
    lincom = c(1, 1, 1),
    variables = "cyl"),
    pattern = "of length")

# errors
expect_error(marginaleffects(
    mod,
    newdata = "mean",
    lincom = matrix(rep(1, 6), ncol = 2),
    variables = "cyl"),
    pattern = "2 rows")

# marginaleffects: lincom
mfx <- marginaleffects(
    mod,
    newdata = "mean",
    variables = "cyl",
    lincom = "pairwise")
expect_inherits(mfx, "marginaleffects")
expect_equivalent(nrow(mfx), 1)


# contrasts: lincom
cmp1 <- comparisons(
    mod,
    variables = "cyl",
    newdata = "mean")
cmp2 <- comparisons(
    mod,
    variables = "cyl",
    newdata = "mean",
    lincom = "pairwise")
expect_equivalent(diff(cmp1$comparison), cmp2$comparison)


# marginaleffects: lincom
mfx <- marginaleffects(
    mod,
    newdata = "mean",
    variables = "cyl",
    lincom = "pairwise")
expect_inherits(mfx, "marginaleffects")
expect_equivalent(nrow(mfx), 1)


# predictions: lincom
p1 <- predictions(
    mod,
    datagrid(cyl = c(4, 6)),
    lincom = c(-1, 1))
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
    lincom = lc)
expect_inherits(p3, "predictions")


# marginalmeans: lincom complex
lc <- c(-2, 1, 1, 0, -1, 1)
em <- emmeans(mod, "carb") 
em <- contrast(em, method = data.frame(custom_contrast = lc))
em <- data.frame(em)
mm <- marginalmeans(mod, variables = "carb", lincom = lc)
expect_equivalent(mm$marginalmean, em$estimate)
expect_equivalent(mm$std.error, em$SE)

# marginalmeans: lincom shortcut
mm <- marginalmeans(mod, variables = "carb", lincom = "reference")
expect_equivalent(nrow(mm), 5)

# marginalmeans: lincom complex matrix
lc <- matrix(c(
    -2, 1, 1, 0, -1, 1,
    -1, 1, 0, 0, 0, 0
    ), ncol = 2)
mm <- marginalmeans(mod, variables = "carb", lincom = lc)
expect_inherits(mm, "marginalmeans")
expect_equal(nrow(mm), 2)


# marginalmeans: string function
mm1 <- marginalmeans(
    mod,
    lincom = "r1 + r2 = 12")
mm2 <- marginalmeans(mod)
expect_equivalent(
    mm2$marginalmean[1] + mm2$marginalmean[2] - 12,
    mm1$marginalmean)


# marginaleffects: string function
mod <- lm(mpg ~ hp + drat, data = mtcars)
mfx1 <- marginaleffects(
    mod,
    newdata = "mean",
    lincom = "exp(r1 + r2) = 100")
mfx2 <- marginaleffects(
    mod,
    newdata = "mean",
    lincom = "exp(hp + drat) = 100")
expect_inherits(mfx1, "marginaleffects")
expect_equivalent(mfx1$dydx, mfx2$dydx)
expect_equivalent(mfx1$std.error, mfx2$std.error)


# predictions: string formulas
p1 <- predictions(
    mod,
    newdata = datagrid(hp = c(100, 110, 120)))
p2 <- predictions(
    mod,
    lincom = "r1 + r2 + r3 = 10",
    newdata = datagrid(hp = c(100, 110, 120)))
p3 <- predictions(
    mod,
    lincom = "r1 = r2",
    newdata = datagrid(hp = c(100, 110, 120)))
expect_equivalent(sum(p1$predicted) - 10, p2$predicted)
expect_equivalent(p1$predicted[1] - p1$predicted[2], p3$predicted)

