source("helpers.R")
using("marginaleffects")
requiet("REndo")

# Basic expectation tests
data("dataLatentIV", package = "REndo")
mod_simple <- REndo::latentIV(y ~ P, data = dataLatentIV, verbose = FALSE)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

data("dataLatentIV", package = "REndo")
mod <- latentIV(y ~ P, data = dataLatentIV, verbose = FALSE)
s <- avg_slopes(mod, type = "response")
expect_inherits(s, "slopes")

data("dataCopCont", package = "REndo")
set.seed(1002)
mod <- copulaCorrection(
    formula = y ~ X1 + X2 + P | continuous(P),
    data = dataCopCont,
    num.boots = 50,
    verbose = FALSE
) |>
    suppressWarnings()
s <- avg_slopes(mod)
expect_inherits(s, "slopes")
