source("helpers.R")
using("marginaleffects")
requiet("survey")

# mtcars logit
tmp <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/mtcars.csv")
tmp$weights <- tmp$w <- 1:32
dat <- tmp
mod <- suppressWarnings(svyglm(
    am ~ mpg + cyl,
    design = svydesign(ids = ~1, weights = ~weights, data = dat),
    family = binomial))

p1 <- predictions(mod, newdata = dat)
p2 <- predictions(mod, wts = "weights", newdata = dat)
p3 <- predictions(mod, wts = "w", newdata = dat)
p4 <- predictions(mod, wts = dat$weights)
expect_false(tidy(p1)$estimate == tidy(p2)$estimate)
expect_false(tidy(p1)$std.error == tidy(p2)$std.error)
expect_equivalent(tidy(p2), tidy(p3))
expect_equivalent(tidy(p2), tidy(p4))


# by supports weights
p1 <- predictions(mod, wts = "weights", newdata = dat)
p1 <- tidy(p1)
expect_inherits(p1, "data.frame")
m1 <- slopes(mod, wts = "weights", newdata = dat, by = "cyl")
m1 <- tidy(m1)
expect_inherits(m1, "data.frame")
c1 <- comparisons(mod, wts = "weights", newdata = dat, by = "cyl")
c1 <- tidy(c1)
expect_inherits(c1, "data.frame")


# wts + comparison="avg"
set.seed(100)
k <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/MatchIt/lalonde.csv")
k$w <- rchisq(614, 2)
fit <- lm(re78 ~ treat * (age + educ + race + married + re74),
          data = k, weights = w)
cmp1 <- comparisons(fit, variables = "treat", wts = "w")
cmp2 <- comparisons(fit, variables = "treat", wts = "w", comparison = "differenceavg")
expect_equivalent(tidy(cmp1)$estimate, weighted.mean(cmp1$estimate, k$w))
expect_equivalent(cmp2$estimate, weighted.mean(cmp1$estimate, k$w))


# sanity check
expect_error(comparisons(mod, wts = "junk"), pattern = "explicitly")
expect_error(slopes(mod, wts = "junk"), pattern = "explicitly")

# vs. Stata (not clear what SE they use, so we give tolerance)
mod <- suppressWarnings(svyglm(
    am ~ mpg,
    design = svydesign(ids = ~1, weights = ~weights, data = dat),
    family = binomial))
tmp <- mod$prior.weights
stata <- c(.0441066, .0061046)
mfx <- slopes(mod, wts = tmp, by = "term")
expect_equivalent(mfx$estimate[1], stata[1], tol = .01)
expect_equivalent(mfx$std.error, stata[2], tolerance = 0.002)



# Issue #737
requiet("tidyverse")
md <- tibble::tribble(
  ~g,   ~device,    ~y,      ~N,                 ~p,
  "Control", "desktop", 12403, 103341L,  0.120020127538925,
  "Control",  "mobile",  1015,  16192L, 0.0626852766798419,
  "Control",  "tablet",    38,    401L, 0.0947630922693267,
  "X", "desktop", 12474, 103063L,  0.121032766366203,
  "X",  "mobile",  1030,  16493L, 0.0624507366761656,
  "X",  "tablet",    47,    438L,  0.107305936073059,
  "Z", "desktop", 12968, 102867L,  0.126065696481865,
  "Z",  "mobile",   973,  16145L, 0.0602663363270362,
  "Z",  "tablet",    34,    438L, 0.0776255707762557,
  "W", "desktop", 12407, 103381L,  0.120012381385361,
  "W",  "mobile",  1007,  16589L,  0.060702875399361,
  "W",  "tablet",    30,    435L, 0.0689655172413793
)
tmp <<- as.data.frame(md)
tmp <- as.data.frame(md)
fit <- glm(cbind(y, N - y) ~ g * device, data = tmp, family = binomial())
cmp1 <- avg_comparisons(fit,
    variables = list(g = c("Control", "Z")),
    wts = "N",
    newdata = tmp,
    transform_pre = "lnratioavg",
    transform_post = exp)
cmp2 <- predictions(fit, variables = list(g = c("Control", "Z"))) |> 
    dplyr::group_by(g) |>
    dplyr::summarise(estimate = weighted.mean(estimate, N)) |>
    as.data.frame()
expect_equivalent(
    cmp1$estimate,
    cmp2$estimate[cmp2$g == "Z"] / cmp2$estimate[cmp2$g == "Control"])

# wts shortcuts are internal-only
expect_error(
    avg_comparisons(fit, variables = "g", wts = "N", comparison = "lnratioavgwts", transform = exp),
    pattern = "check_choice"
)

# lnratioavg = lnratio with `by`
cmp1 <- avg_comparisons(fit,
    variables = "g",
    by = "device",
    wts = "N",
    comparison = "lnratioavg",
    transform = exp)
cmp2 <- avg_comparisons(fit,
    variables = "g",
    by = "device",
    wts = "N",
    comparison = "lnratio",
    transform = exp)
expect_equivalent(cmp1, cmp2)

# lnratioavg + wts produces same results in this particular case, because there are only the g*device predictors
cmp1 <- avg_comparisons(fit,
    variables = "g",
    by = "device",
    wts = "N",
    comparison = "lnratioavg",
    transform = exp)
cmp2 <- avg_comparisons(fit,
    variables = "g",
    by = "device",
    wts = "N",
    comparison = "lnratioavg",
    transform = exp)
expect_equivalent(cmp1, cmp2)



# brms
set.seed(1024)
mod <- marginaleffects:::modelarchive_model("brms_numeric2")
w <- runif(32)
cmp1 <- comparisons(mod, comparison = "differenceavg")
cmp2 <- comparisons(mod, wts = w, comparison = "differenceavg")
expect_true(all(cmp1$estimate != cmp2$estimate))

# . logit am mpg [pw=weights]
#
# Iteration 0:   log pseudolikelihood = -365.96656  
# Iteration 1:   log pseudolikelihood = -255.02961  
# Iteration 2:   log pseudolikelihood = -253.55843  
# Iteration 3:   log pseudolikelihood = -253.55251  
# Iteration 4:   log pseudolikelihood = -253.55251  
#
# Logistic regression                                     Number of obs =     32
#                                                         Wald chi2(1)  =   8.75
#                                                         Prob > chi2   = 0.0031
# Log pseudolikelihood = -253.55251                       Pseudo R2     = 0.3072
#
# ------------------------------------------------------------------------------
#              |               Robust
#           am | Coefficient  std. err.      z    P>|z|     [95% conf. interval]
# -------------+----------------------------------------------------------------
#          mpg |   .2789194   .0943021     2.96   0.003     .0940908    .4637481
#        _cons |  -5.484059   2.066303    -2.65   0.008    -9.533938   -1.434179
# ------------------------------------------------------------------------------
#
# . margins, dydx(mpg)
#
# Average marginal effects                                    Number of obs = 32
# Model VCE: Robust
#
# Expression: Pr(am), predict()
# dy/dx wrt:  mpg
#
# ------------------------------------------------------------------------------
#              |            Delta-method
#              |      dy/dx   std. err.      z    P>|z|     [95% conf. interval]
# -------------+----------------------------------------------------------------
#          mpg |   .0441066   .0061046     7.23   0.000     .0321419    .0560714
# ------------------------------------------------------------------------------




source("helpers.R")
rm(list = ls())