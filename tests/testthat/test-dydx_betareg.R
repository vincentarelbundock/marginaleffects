skip_if_not_installed("betareg")

library("margins")

test_that("betareg", {
    data("GasolineYield", package = "betareg")
    mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    suppressWarnings({
        res <- marginsxp(mod, variables = "temp", variance = NULL)
        mar <- data.frame(margins(mod, unit_ses = FALSE))
    })
    expect_true(cor(as.numeric(mar$temp), res$temp, use = "complete.obs") > .99999)
})
