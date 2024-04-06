source("helpers.R")
using("marginaleffects")
set.seed(1024)

tol <- tolse <- 1e-3

results <- readRDS(testing_path("stata/stata.rds"))

# lm()
dat <- read.csv(testing_path("stata/databases/stats_lm_01.csv"))
mod <- lm(y ~ x1 * x2, data = dat)

mfx <- slopes(mod, slope = "eyex")
expect_inherits(mfx, "slopes")
mfx <- avg_slopes(mod, slope = "eyex")
sta <- results$stats_lm_elasticity_eyex
expect_equivalent(mfx$estimate, sta$dydxstata)


expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)
mfx <- avg_slopes(mod, slope = "eydx", numderiv = "richardson")
sta <- results$stats_lm_elasticity_eydx
expect_equivalent(mfx$estimate, sta$dydxstata)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

mfx <- avg_slopes(mod, slope = "dyex", numderiv = "richardson")
sta <- results$stats_lm_elasticity_dyex
expect_equivalent(mfx$estimate, sta$dydxstata)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

# glm()
dat <- read.csv(testing_path("stata/databases/stats_glm_01.csv"))
mod <- glm(y ~ x1 * x2, data = dat, family = binomial)

mfx <- avg_slopes(mod, slope = "eyex", numderiv = "richardson")
sta <- results$stats_glm_elasticity_eyex
expect_equivalent(mfx$estimate, sta$dydxstata, tolerance = tol)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

mfx <- avg_slopes(mod, slope = "eydx", numderiv = "richardson")
sta <- results$stats_glm_elasticity_eydx
expect_equivalent(mfx$estimate, sta$dydxstata, tolerance = tol)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

mfx <- avg_slopes(mod, slope = "dyex", numderiv = "richardson")
sta <- results$stats_glm_elasticity_dyex
expect_equivalent(mfx$estimate, sta$dydxstata, tolerance = 1e-2)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)



source("helpers.R")
rm(list = ls())