skip_if_not_installed("betareg")

library("margins")
library("haven")
library("betareg")
library("dplyr")

test_that("betareg", {
    data("GasolineYield", package = "betareg")
    mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    suppressWarnings({
        res <- meffects(mod, variables = "temp", variance = NULL)
        mar <- data.frame(margins(mod, unit_ses = FALSE))
    })
    expect_true(cor(as.numeric(mar$temp), res$temp, use = "complete.obs") > .99999)
})


test_that("betareg vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))[["betareg_betareg_01"]]
    dat <- read_dta(test_path("stata/data/betareg_betareg_01.dta"))
    mod <- betareg::betareg(yield ~ factor(batch) + temp, data = dat)
    mfx <- suppressWarnings(meffects(mod, variance = NULL)) %>%
           group_by(term) %>%
           summarize(dydx = mean(dydx)) %>%
           inner_join(stata)
    expect_equal(ame$dydx, ame$dydxstata, tolerance = 0.00001)
})
