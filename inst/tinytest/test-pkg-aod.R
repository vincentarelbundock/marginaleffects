source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("aod")

# betabin: no validity
data("orob2", package = "aod")
mod <- betabin(cbind(y, n - y) ~ seed, ~ 1, data = orob2)
expect_marginaleffects(mod, n_unique = 1)
expect_predictions(predictions(mod))

