skip_if_not_installed("betareg")
skip_if_not_installed("margins")
requiet("dplyr")

test_that("betareg vs. margins", {
    data("GasolineYield", package = "betareg")
    mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    suppressWarnings({
        res <- marginaleffects(mod, variables = "temp")
        mar <- data.frame(margins::margins(mod, unit_ses = TRUE))
    })
    expect_true(test_against_margins(res, mar, tolerance = 0.1))
    warning("low tolerance")
})

test_that("betareg vs. Stata", {
    skip("betareg vs. Stata: matching mfx but different std.errors")
    stata <- readRDS(test_path("stata/stata.rds"))[["betareg_betareg_01"]]
    dat <- read.csv(test_path("stata/databases/betareg_betareg_01.csv"))
    mod <- betareg::betareg(yield ~ factor(batch) + temp, data = dat)
    ame <- suppressWarnings(marginaleffects(mod)) %>%
           group_by(term) %>%
           summarize(dydx = mean(dydx), std.error = mean(std.error)) %>%
           inner_join(stata, by = "term")
    expect_equal(ame$dydx, ame$dydxstata, tolerance = 0.00001)
    expect_equal(ame$std.error, ame$std.errorstata, tolerance = 0.00001)
})
