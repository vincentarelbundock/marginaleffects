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
    null = null,
    delta = delta,
    side = "noninferiority") |>
    dplyr::arrange(gear)
expect_equivalent(e1$z.ratio, e2$statistic)
expect_equivalent(e1$p.value, e2$p.value)


# predictions() vs. {emmeans}: sup
delta <- 1
null <- 23 
e1 <- test(em, delta = delta, null = null, side = "nonsuperiority", df = Inf)
e2 <- predictions(
    mod,
    newdata = datagrid(gear = unique),
    null = null,
    delta = delta,
    side = "nonsuperiority") |>
    dplyr::arrange(gear)
expect_equivalent(e1$z.ratio, e2$statistic)
expect_equivalent(e1$p.value, e2$p.value)


# predictions() vs. {emmeans}: equiv
delta <- 1
null <- 22 
e1 <- test(em, delta = delta, null = null, side = "equivalence", df = Inf)
e2 <- predictions(
    mod,
    newdata = datagrid(gear = unique),
    null = null,
    delta = delta,
    side = "equivalence") |>
    dplyr::arrange(gear)
expect_equivalent(e1$z.ratio, e2$statistic)
expect_equivalent(e1$p.value, e2$p.value)


# slopes() works; no validity
mfx <- slopes(
    mod,
    variables = "hp",
    newdata = "mean",
    side = "equivalence",
    delta = .05,
    null = -.04)
expect_inherits(mfx, "slopes")


# marginalmeans() vs. {emmeans}
mod <- lm(log(conc) ~ source + factor(percent), data = pigs)
rg <- ref_grid(mod)
em <- emmeans(rg, "source", at = list(), df = Inf)
pa <- pairs(em, df = Inf)
mm <- marginalmeans(
    mod,
    variables = "source",
    hypothesis = "pairwise") 

e1 <- test(pa, delta = delta, adjust = "none", side = "nonsuperiority", df = Inf)
e2 <- hypotheses(mm, delta = delta, side = "nonsuperiority")
expect_equivalent(e1$z.ratio, e2$statistic)
expect_equivalent(e1$p.value, e2$p.value)

e1 <- test(pa, delta = delta, adjust = "none", side = "noninferiority", df = Inf)
e2 <- hypotheses(mm, delta = delta, side = "noninferiority")
expect_equivalent(e1$z.ratio, e2$statistic)
expect_equivalent(e1$p.value, e2$p.value)

e1 <- test(pa, delta = delta, adjust = "none", df = Inf)
e2 <- hypotheses(mm, delta = delta, side = "equivalence")
expect_equivalent(e1$z.ratio, e2$statistic)
expect_equivalent(e1$p.value, e2$p.value)


# two-sample t-test
requiet("equivalence")
set.seed(1024)
N <- 100
delta <- 0.05
dat <- rbind(data.frame(y = rnorm(N), x = 0),
             data.frame(y = rnorm(N, mean = 0.3), x = 1))
mod <- lm(y ~ x, data = dat)
FUN <- function(model, ...) {
    data.frame(term = "t-test", estimate = coef(model)[2])
}
e1 <- tost(dat$y[dat$x == 0], dat$y[dat$x == 1], epsilon = delta)
e2 <- hypotheses(
    mod,
    FUN = FUN,
    side = "equivalence",
    null = 0,
    delta = delta,
    df = e1$parameter)
expect_true(e1$tost.p.value > .5 && e1$tost.p.value < .9)
expect_equivalent(e1$tost.p.value, e2$p.value)