# TODO: test non-linear GLM to know if t is computed on the link scale
source("helpers.R")
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
    newdata = datagrid(gear = unique),
    equivalence = c(19, 21),
    side = "noninferiority") |>
    dplyr::arrange(gear)
expect_equivalent(e1$z.ratio, e2$statistic.inf)
expect_equivalent(e1$p.value, e2$p.value.inf)


# predictions() vs. {emmeans}: sup
e1 <- test(em, delta = 1, null = 23, side = "nonsuperiority", df = Inf)
e2 <- predictions(
    mod,
    newdata = datagrid(gear = unique),
    equivalence = c(22, 24)) |>
    dplyr::arrange(gear)
expect_equivalent(e1$z.ratio, e2$statistic.sup)
expect_equivalent(e1$p.value, e2$p.value.sup)


# predictions() vs. {emmeans}: equiv
e1 <- test(em, delta = 1, null = 22, side = "equivalence", df = Inf)
e2 <- predictions(
    mod,
    newdata = datagrid(gear = unique),
    equivalence = c(21, 23)) |>
    dplyr::arrange(gear)
expect_equivalent(e1$p.value, e2$p.value.equ)


# slopes() works; no validity
mfx <- slopes(
    mod,
    variables = "hp",
    newdata = "mean",
    equivalence = c(-.09, .01))
expect_inherits(mfx, "slopes")


# marginalmeans() vs. {emmeans}
delta <- log(1.25)
mod <- lm(log(conc) ~ source + factor(percent), data = pigs)
rg <- ref_grid(mod)
em <- emmeans(rg, "source", at = list(), df = Inf)
pa <- pairs(em, df = Inf)
mm <- marginalmeans(
    mod,
    variables = "source",
    hypothesis = "pairwise") 

e1 <- test(pa, delta = delta, adjust = "none", side = "nonsuperiority", df = Inf)
e2 <- hypotheses(mm, equivalence = c(-delta, delta))
expect_equivalent(e1$z.ratio, e2$statistic.sup)
expect_equivalent(e1$p.value, e2$p.value.sup)

e1 <- test(pa, delta = delta, adjust = "none", side = "noninferiority", df = Inf)
e2 <- hypotheses(mm, equivalence = c(-delta, delta))
expect_equivalent(e1$z.ratio, e2$statistic.inf)
expect_equivalent(e1$p.value, e2$p.value.inf)

e1 <- test(pa, delta = delta, adjust = "none", df = Inf)
e2 <- hypotheses(mm, equivalence = c(-delta, delta))
expect_equivalent(e1$p.value, e2$p.value.equ)


# two-sample t-test
requiet("equivalence")
set.seed(1024)
N <- 100
dat <- rbind(data.frame(y = rnorm(N), x = 0),
             data.frame(y = rnorm(N, mean = 0.3), x = 1))
mod <- lm(y ~ x, data = dat)
FUN <- function(model, ...) {
    data.frame(term = "t-test", estimate = coef(model)[2])
}
e1 <- tost(dat$y[dat$x == 0], dat$y[dat$x == 1], epsilon = .05)
e2 <- hypotheses(
    mod,
    FUN = FUN,
    equivalence = c(-.05, .05),
    df = e1$parameter)
expect_true(e1$tost.p.value > .5 && e1$tost.p.value < .9)
expect_equivalent(e1$tost.p.value, e2$p.value.equ)

# avg_*() and hypotheses()
tmp <<- lm(mpg ~ hp * qsec, data = mtcars)
cmp <- avg_comparisons(tmp) |> hypotheses(equivalence = c(-.2, 0))
mfx <- avg_slopes(tmp) |> hypotheses(equivalence = c(-.2, 0))
pre <- avg_predictions(tmp) |> hypotheses(equivalence = c(-.2, 0))
expect_inherits(cmp, "hypotheses")
expect_inherits(mfx, "hypotheses")
expect_inherits(pre, "hypotheses")

# exit_if_not(require("tinyviztest"))
cmp <- avg_comparisons(tmp, equivalence = c(-.1, 0))
expect_snapshot_print(cmp, "equivalence-avg_comparisons")
