source("helpers.R")
requiet("REndo")
requiet("marginaleffects")

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
