requiet("logitr")
requiet("marginaleffects")

mod <- suppressMessages(logitr(
    data    = yogurt,
    outcome = "choice",
    obsID   = "obsID",
    pars    = c("price", "feat", "brand")))

p <- predictions(mod)
expect_inherits(p, "predictions")

mfx <- marginaleffects(mod)
expect_inherits(mfx, "marginaleffects")

cmp <- comparisons(mod)
expect_inherits(cmp, "comparisons")