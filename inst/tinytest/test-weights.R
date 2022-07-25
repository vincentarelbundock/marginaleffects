source("helpers.R")
requiet("survey")

# mtcars logit
dat <- mtcars
dat$weights <- dat$w <- 1:32
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
expect_equal(tidy(p2), tidy(p3))
expect_equal(tidy(p2), tidy(p4))


# by supports weights
p1 <- predictions(mod, wts = "weights", newdata = dat)
p1 <- tidy(p1)
expect_inherits(p1, "data.frame")
m1 <- marginaleffects(mod, wts = "weights", newdata = dat, by = "cyl")
m1 <- tidy(m1)
expect_inherits(m1, "data.frame")
c1 <- comparisons(mod, wts = "weights", newdata = dat, by = "cyl")
c1 <- tidy(c1)
expect_inherits(c1, "data.frame")


# sanity check
expect_error(comparisons(mod, wts = "junk"), pattern = "explicitly")
expect_error(marginaleffects(mod, wts = "junk"), pattern = "explicitly")

# vs. Stata (not clear what SE they use, so we give tolerance)
mod <- suppressWarnings(svyglm(
    am ~ mpg,
    design = svydesign(ids = ~1, weights = ~weights, data = dat),
    family = binomial))
stata <- c("estimate" = .0441066, "std.error" = .0061046)
mfx <- marginaleffects(mod, wts = mod$prior.weights)
mfx <- tidy(mfx)
mfx <- unlist(mfx[, 3:4])
expect_equivalent(mfx, stata, tolerance = 0.0002)

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
