source("helpers.R")
using("marginaleffects")

requiet("aod")

# Basic expectation tests
dat_simple <- get_dataset("orob2", "aod")
dat_simple$seed <- factor(dat_simple$seed)
mod_simple <- aod::betabin(cbind(y, n - y) ~ seed, ~1, data = dat_simple)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

# betabin: no validity
dat <- get_dataset("orob2", "aod")

# character variables should be padded, but I am lazy
mod <- betabin(cbind(y, n - y) ~ seed, ~1, data = dat)
expect_error(slopes(mod), pattern = "support.*character")

# factor variables work
dat$seed <- factor(dat$seed)
mod <- betabin(cbind(y, n - y) ~ seed, ~1, data = dat)
expect_slopes(mod, n_unique = 1)

expect_predictions(mod)
