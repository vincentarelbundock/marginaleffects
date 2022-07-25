source("helpers.R")
set.seed(1024)

tol <- 1e-4
tolse <- 1e-2
eps <- 1e-6

results <- readRDS(testing_path("stata/stata.rds"))

# lm()
dat <- read.csv(testing_path("stata/databases/stats_lm_01.csv"))
mod <- lm(y ~ x1 * x2, data = dat)

mfx <- tidy(marginaleffects(mod, slope = "eyex"))
sta <- results$stats_lm_elasticity_eyex
expect_equivalent(mfx$estimate, sta$dydxstata)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

mfx <- tidy(marginaleffects(mod, slope = "eydx", eps = eps))
sta <- results$stats_lm_elasticity_eydx
expect_equivalent(mfx$estimate, sta$dydxstata)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

mfx <- tidy(marginaleffects(mod, slope = "dyex", eps = eps))
sta <- results$stats_lm_elasticity_dyex
expect_equivalent(mfx$estimate, sta$dydxstata)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

# glm()
dat <- read.csv(testing_path("stata/databases/stats_glm_01.csv"))
mod <- glm(y ~ x1 * x2, data = dat, family = binomial)

mfx <- tidy(marginaleffects(mod, slope = "eyex", eps = eps))
sta <- results$stats_glm_elasticity_eyex
expect_equivalent(mfx$estimate, sta$dydxstata, tolerance = tol)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

mfx <- tidy(marginaleffects(mod, slope = "eydx", eps = eps))
sta <- results$stats_glm_elasticity_eydx
expect_equivalent(mfx$estimate, sta$dydxstata, tolerance = tol)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

mfx <- tidy(marginaleffects(mod, slope = "dyex", eps = eps))
sta <- results$stats_glm_elasticity_dyex
expect_equivalent(mfx$estimate, sta$dydxstata, tolerance = tol)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)


