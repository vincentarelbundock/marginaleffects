testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("parameters")
requiet("dplyr")
requiet("emmeans")
requiet("parameters")

mod <- lm(mpg ~ hp + factor(gear), data = mtcars)

# predictions() vs. {emmeans}: inf
delta <- 1
null <- 20
em <- emmeans(mod, "gear", df = Inf)
e1 <- test(em, delta = delta, null = null, side = "noninferiority", df = Inf)
e2 <- predictions(
    mod,
    # emmeans treats integers as floats
    newdata = datagrid(gear = unique, FUN_integer = mean),
    equivalence = c(19, 21)
) |> dplyr::arrange(gear)
expect_equal(e1$z.ratio, e2$statistic.noninf, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(e1$p.value, e2$p.value.noninf, tolerance = 1e-6, ignore_attr = TRUE)

e2 <- predictions(
    mod,
    by = "gear",
    newdata = datagrid(grid_type = "balanced"),
    equivalence = c(19, 21)
) |> dplyr::arrange(gear)

# predictions() vs. {emmeans}: sup
e1 <- test(em, delta = 1, null = 23, side = "nonsuperiority", df = Inf)
e2 <- predictions(
    mod,
    # emmeans treats integers as floats
    newdata = datagrid(gear = unique, FUN_integer = mean),
    equivalence = c(22, 24)
) |>
    dplyr::arrange(gear)
expect_equal(e1$z.ratio, e2$statistic.nonsup, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(e1$p.value, e2$p.value.nonsup, tolerance = 1e-6, ignore_attr = TRUE)


# predictions() vs. {emmeans}: equiv
e1 <- test(em, delta = 1, null = 22, side = "equivalence", df = Inf)
e2 <- predictions(
    mod,
    # emmeans treats integers as floats
    newdata = datagrid(gear = unique, FUN_integer = mean),
    equivalence = c(21, 23)
) |>
    dplyr::arrange(gear)
expect_equal(e1$p.value, e2$p.value.equiv, tolerance = 1e-6, ignore_attr = TRUE)


# slopes() works; no validity
mfx <- slopes(
    mod,
    variables = "hp",
    newdata = "mean",
    equivalence = c(-.09, .01)
)
expect_s3_class(mfx, "slopes")


# two-sample t-test
testthat::skip_if_not_installed("equivalence")
requiet("equivalence")
set.seed(1024)
N <- 100
dat_equivalence <- rbind(
    data.frame(y = rnorm(N), x = 0),
    data.frame(y = rnorm(N, mean = 0.3), x = 1)
)
mod <- lm(y ~ x, data = dat_equivalence)
FUN <- function(x) {
    data.frame(term = "t-test", estimate = coef(x)[2])
}
e1 <- tost(dat_equivalence$y[dat_equivalence$x == 0], dat_equivalence$y[dat_equivalence$x == 1], epsilon = .05)
e2 <- hypotheses(
    mod,
    hypothesis = FUN,
    equivalence = c(-.05, .05),
    df = e1$parameter
)
expect_true(e1$tost.p.value > .5 && e1$tost.p.value < .9)
expect_equal(e1$tost.p.value, e2$p.value.equiv, ignore_attr = TRUE)


# GLM vs emmeans
mod <- glm(vs ~ factor(gear), data = mtcars, family = binomial)
em <- emmeans(mod, "gear", df = Inf)
e1 <- test(em, delta = .5, null = 1, side = "noninferiority", df = Inf)
e2 <- predictions(
    mod,
    type = "link",
    newdata = datagrid(gear = unique),
    equivalence = c(.5, 1.5),
    numderiv = "richardson"
) |>
    dplyr::arrange(gear)
expect_equal(e1$emmean, e2$estimate, ignore_attr = TRUE)
expect_equal(e1$z.ratio, e2$statistic.noninf, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(e1$p.value, e2$p.value.noninf, tolerance = 1e-6, ignore_attr = TRUE)


# avg_*() and hypotheses()
tmp <- lm(mpg ~ hp * qsec, data = mtcars)
cmp <- avg_comparisons(tmp) |> hypotheses(equivalence = c(-.2, 0))
mfx <- avg_slopes(tmp) |> hypotheses(equivalence = c(-.2, 0))
pre <- avg_predictions(tmp) |> hypotheses(equivalence = c(-.2, 0))
expect_s3_class(cmp, "hypotheses")
expect_s3_class(mfx, "hypotheses")
expect_s3_class(pre, "hypotheses")
cmp <- avg_comparisons(tmp, equivalence = c(-.1, 0))
expect_snapshot(cmp)


# bug on with call and symbols
mod <- lm(mpg ~ hp * vs, data = mtcars)
x <- avg_slopes(mod, by = "vs", variables = "hp", hypothesis = ~pairwise)
x <- hypotheses(x, equivalence = c(-.2, .2))
expect_s3_class(x, "hypotheses")


rm("mod")
delta <- log(1.25)
data(pigs, package = "emmeans")
mod <- lm(log(conc) ~ source + factor(percent), data = pigs)
rg <- ref_grid(mod)
em <- emmeans(rg, "source", at = list(), df = Inf)
pa <- pairs(em, df = Inf)

mm <- predictions(
    mod,
    newdata = datagrid(grid_type = "balanced"),
    by = "source",
    hypothesis = ~pairwise,
    transform = \(x) -x
)

e1 <- test(pa, delta = delta, adjust = "none", side = "nonsuperiority", df = Inf)
e2 <- hypotheses(mm, equivalence = c(-delta, delta))
expect_equal(e1$z.ratio, e2$statistic.nonsup, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(e1$p.value, e2$p.value.nonsup, tolerance = 1e-6, ignore_attr = TRUE)

e1 <- test(pa, delta = delta, adjust = "none", side = "noninferiority", df = Inf)
e2 <- hypotheses(mm, equivalence = c(-delta, delta))
expect_equal(e1$z.ratio, e2$statistic.noninf, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(e1$p.value, e2$p.value.noninf, tolerance = 1e-6, ignore_attr = TRUE)

e1 <- test(pa, delta = delta, adjust = "none", df = Inf)
e2 <- hypotheses(mm, equivalence = c(-delta, delta))
expect_equal(e1$p.value, e2$p.value.equiv, tolerance = 1e-6, ignore_attr = TRUE)
