source("helpers.R")
using("marginaleffects")
exit_if_not(requiet("survey"))

# mtcars logit
tmp <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/mtcars.csv")
tmp$weights <- tmp$w <- 1:32
dat <<- tmp
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


# wts + transform_pre="avg"
set.seed(100)
k <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/MatchIt/lalonde.csv")
k$w <- rchisq(614, 2)
fit <- lm(re78 ~ treat * (age + educ + race + married + re74),
          data = k, weights = w)
cmp1 <- comparisons(fit, variables = "treat", wts = "w")
cmp2 <- comparisons(fit, variables = "treat", wts = "w", transform_pre = "differenceavg")
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



# brms
set.seed(1024)
mod <- marginaleffects:::modelarchive_model("brms_numeric2")
w <- runif(32)
cmp1 <- comparisons(mod, transform_pre = "differenceavg")
cmp2 <- comparisons(mod, wts = w, transform_pre = "differenceavg")
cmp3 <- comparisons(mod, wts = w, transform_pre = "differenceavgwts")
expect_true(all(cmp1$estimate != cmp2$estimate))
expect_equivalent(cmp2$estimate, cmp3$estimate)

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



