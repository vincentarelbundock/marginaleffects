source("helpers.R")
requiet("margins")

dat <- mtcars
dat$weights <- 1:32
mod <- glm(
    am ~ mpg, data = dat,
    family = binomial,
    weights = weights)

# sanity check
expect_error(comparisons(mod, weights = "junk"), pattern = "explicitly")
expect_error(marginaleffects(mod, weights = "junk"), pattern = "explicitly")

# vs. Stata (not clear what SE they use, so we give tolerance)
stata <- c("estimate" = .0441066, "std.error" = .0061046)
mfx <- marginaleffects(mod, weights = "weights", vcov = "HC1")
mfx <- tidy(mfx)
mfx <- unlist(mfx[, 3:4])
expect_equivalent(mfx, stata, tolerance = 0.002)


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
