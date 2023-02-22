source("helpers.R")
using("marginaleffects")

requiet("speedglm")
requiet("margins")

# glm vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))[["stats_glm_01"]]
dat <- read.csv(testing_path("stata/databases/stats_glm_01.csv"))
mod <- speedglm(y ~ x1 * x2, family = binomial(), data = dat)
mfx <- merge(tidy(slopes(mod)), stata)
expect_slopes(mod)
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = .0001)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .0001)

# margins: wrong standard errors
mfx <- slopes(mod)
mar <- margins(mod, unit_ses = TRUE)
expect_true(expect_margins(mfx, mar, tolerance = .001))

# lm vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))[["stats_lm_01"]]
dat <- read.csv(testing_path("stata/databases/stats_lm_01.csv"))
mod <- speedlm(y ~ x1 * x2, data = dat)
mfx <- merge(tidy(slopes(mod)), stata)
expect_slopes(mod)
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = .00001)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .0001)

# margins: wrong standard errors
mfx <- slopes(mod)
mar <- margins(mod, unit_ses = TRUE)
expect_true(expect_margins(mfx, mar, tolerance = .0001))



rm(list = ls())