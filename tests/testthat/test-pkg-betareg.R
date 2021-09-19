skip_if_not_installed("betareg")
skip_if_not_installed("margins")
requiet("dplyr")

test_that("betareg vs. margins", {
    set.seed(1024)
    data("GasolineYield", package = "betareg")
    mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    suppressWarnings({
        res <- marginaleffects(mod, variables = "temp")
        mar <- data.frame(margins::margins(mod, unit_ses = TRUE))
    })
    expect_true(test_against_margins(res, mar, tolerance = 0.1))
})


test_that("betareg vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))[["betareg_betareg_01"]]
    dat <- read.csv(test_path("stata/databases/betareg_betareg_01.csv"))
    mod <- betareg::betareg(yield ~ factor(batch) + temp, data = dat)
    mfx <- merge(tidy(marginaleffects(mod)), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)
})
