skip_if_not_installed("betareg")

test_that("betareg", {
    library(betareg)
    data("GasolineYield", package = "betareg")
    mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    suppressWarnings({
        res <- mfx(mod, variables = "temp", variance = NULL)
        mar <- data.frame(margins(mod, unit_ses = FALSE))
    })
    expect_true(cor(as.numeric(mar$temp), res$temp, use = "complete.obs") > .99999)
    # TODO: variance does not work for betareg objects
})
