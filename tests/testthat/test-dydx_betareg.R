skip_if_not_installed("betareg")

library("margins")
library("haven")
library("betareg")
library("dplyr", warn.conflicts = FALSE)

test_that("betareg", {
    data("GasolineYield", package = "betareg")
    mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    suppressWarnings({
        res <- meffects(mod, variables = "temp")
        mar <- data.frame(margins(mod, unit_ses = TRUE))
    })
    expect_true(cor(mar$dydx_temp, res$dydx) > .99999)
    expect_true(cor(mar$SE_dydx_temp, res$std.error) > .999)
    test_against_margins(res, mar)
})

test_that("betareg vs. Stata", {
    skip("betareg vs. Stata: matching mfx but different std.errors")
    stata <- readRDS(test_path("stata/stata.rds"))[["betareg_betareg_01"]]
    dat <- read_dta(test_path("stata/data/betareg_betareg_01.dta"))
    mod <- betareg::betareg(yield ~ factor(batch) + temp, data = dat)
    ame <- suppressWarnings(meffects(mod)) %>%
           group_by(term) %>%
           summarize(dydx = mean(dydx), std.error = mean(std.error)) %>%
           inner_join(stata, by = "term")
    expect_equal(ame$dydx, ame$dydxstata, tolerance = 0.00001)
    expect_equal(ame$std.error, ame$std.errorstata, tolerance = 0.00001)
})
