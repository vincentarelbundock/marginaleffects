source("helpers.R")
using("marginaleffects")

requiet("aod")

# betabin: no validity
data("orob2", package = "aod")
mod <- betabin(cbind(y, n - y) ~ seed, ~ 1, data = orob2)
mfx <- marginaleffects(mod)
expect_marginaleffects(mfx, n_unique = 1)

pre <- predictions(mod)
expect_predictions(pre)

