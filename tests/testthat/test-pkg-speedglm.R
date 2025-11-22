testthat::skip_if_not_installed("speedglm")
testthat::skip_if_not_installed("margins")
requiet("speedglm")
requiet("margins")

# Basic expectation tests
mod_simple <- speedglm::speedglm(am ~ mpg + wt, data = mtcars, family = binomial())
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# glm vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))[["stats_glm_01"]]
dat_speedglm1 <- read.csv(testing_path("stata/databases/stats_glm_01.csv"))
mod <- speedglm(y ~ x1 * x2, family = binomial(), data = dat_speedglm1)
mfx <- merge(avg_slopes(mod), stata)
expect_slopes2(mod)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001, ignore_attr = TRUE)

# margins: wrong standard errors
mfx <- slopes(mod)
mar <- margins(mod, unit_ses = TRUE)
expect_margins2(mfx, mar, tolerance = .001)

# lm vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))[["stats_lm_01"]]
dat_speedglm2 <- read.csv(testing_path("stata/databases/stats_lm_01.csv"))
mod <- speedlm(y ~ x1 * x2, data = dat_speedglm2)
mfx <- merge(avg_slopes(mod), stata)
expect_slopes2(mod)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .00001, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001, ignore_attr = TRUE)

# margins: wrong standard errors
mfx <- slopes(mod)
mar <- margins(mod, unit_ses = TRUE)
expect_margins2(mfx, mar, tolerance = 1e-3)
