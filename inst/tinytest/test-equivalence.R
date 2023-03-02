source("helpers.R")
requiet("poorman")
requiet("emmeans")
requiet("parameters")
# exit_file("TODO")

mod <- lm(mpg ~ hp + factor(gear), data = mtcars)

# predictions() vs. {emmeans}: inf
delta <- 1
null <- 20 
em <- emmeans(mod, "gear", df = Inf)
e1 <- test(em, delta = delta, null = null, side = "noninferiority", df = Inf)
e2 <- predictions(
    mod,
    newdata = datagrid(gear = unique),
    equivalence = c(19, 21)) |>
    poorman::arrange(gear)
expect_equivalent(e1$z.ratio, e2$statistic.noninf)
expect_equivalent(e1$p.value, e2$p.value.noninf)


# predictions() vs. {emmeans}: sup
e1 <- test(em, delta = 1, null = 23, side = "nonsuperiority", df = Inf)
e2 <- predictions(
    mod,
    newdata = datagrid(gear = unique),
    equivalence = c(22, 24)) |>
    poorman::arrange(gear)
expect_equivalent(e1$z.ratio, e2$statistic.nonsup)
expect_equivalent(e1$p.value, e2$p.value.nonsup)


# predictions() vs. {emmeans}: equiv
e1 <- test(em, delta = 1, null = 22, side = "equivalence", df = Inf)
e2 <- predictions(
    mod,
    newdata = datagrid(gear = unique),
    equivalence = c(21, 23)) |>
    poorman::arrange(gear)
expect_equivalent(e1$p.value, e2$p.value.equiv)


# slopes() works; no validity
mfx <- slopes(
    mod,
    variables = "hp",
    newdata = "mean",
    equivalence = c(-.09, .01))
expect_inherits(mfx, "slopes")



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
expect_equivalent(e1$tost.p.value, e2$p.value.equiv)


# GLM vs emmeans
mod <- glm(vs ~ factor(gear), data = mtcars, family = binomial)
em <- emmeans(mod, "gear", df = Inf)
e1 <- test(em, delta = .5, null = 1, side = "noninferiority", df = Inf)
e2 <- predictions(
    mod,
    type = "link",
    newdata = datagrid(gear = unique),
    equivalence = c(.5, 1.5)) |>
    poorman::arrange(gear)
expect_equivalent(e1$emmean, e2$estimate)
expect_equivalent(e1$z.ratio, e2$statistic.noninf)
expect_equivalent(e1$p.value, e2$p.value.noninf)


# avg_*() and hypotheses()
tmp <- lm(mpg ~ hp * qsec, data = mtcars)
cmp <- avg_comparisons(tmp) |> hypotheses(equivalence = c(-.2, 0))
mfx <- avg_slopes(tmp) |> hypotheses(equivalence = c(-.2, 0))
pre <- avg_predictions(tmp) |> hypotheses(equivalence = c(-.2, 0))
expect_inherits(cmp, "hypotheses")
expect_inherits(mfx, "hypotheses")
expect_inherits(pre, "hypotheses")

if (!requiet("tinysnapshot")) {
    exit_file("tinysnapshot")
}
cmp <- avg_comparisons(tmp, equivalence = c(-.1, 0))
expect_snapshot_print(cmp, "equivalence-avg_comparisons")


# bug on with call and symbols
mod <- lm(mpg ~ hp * vs, data = mtcars)
x <- avg_slopes(mod, by = "vs", variables = "hp", hypothesis = "pairwise")
x <- hypotheses(x, equivalence = c(-.2, .2))
expect_inherits(x, "hypotheses")



# marginal_means() vs. {emmeans}
exit_file("works interactively")
delta <- log(1.25)
mod <- lm(log(conc) ~ source + factor(percent), data = pigs)
rg <- ref_grid(mod)
em <- emmeans(rg, "source", at = list(), df = Inf)
pa <- pairs(em, df = Inf)
mm <- marginal_means(
    mod,
    variables = "source",
    hypothesis = "pairwise") 

e1 <- test(pa, delta = delta, adjust = "none", side = "nonsuperiority", df = Inf)
e2 <- hypotheses(mm, equivalence = c(-delta, delta))
expect_equivalent(e1$z.ratio, e2$statistic.nonsup)
expect_equivalent(e1$p.value, e2$p.value.nonsup)

e1 <- test(pa, delta = delta, adjust = "none", side = "noninferiority", df = Inf)
e2 <- hypotheses(mm, equivalence = c(-delta, delta))
expect_equivalent(e1$z.ratio, e2$statistic.noninf)
expect_equivalent(e1$p.value, e2$p.value.noninf)

e1 <- test(pa, delta = delta, adjust = "none", df = Inf)
e2 <- hypotheses(mm, equivalence = c(-delta, delta))
expect_equivalent(e1$p.value, e2$p.value.equiv)


rm(list = ls())