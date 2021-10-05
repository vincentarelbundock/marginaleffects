skip_if_not_installed("betareg")
skip_if_not_installed("margins")
skip_if_not_installed("emmeans")
skip_if_not_installed("broom")

test_that("marginaleffects: vs. margins", {
    set.seed(1024)
    data("GasolineYield", package = "betareg")
    mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    suppressWarnings({
        res <- marginaleffects(mod, variables = "temp")
        mar <- data.frame(margins::margins(mod, unit_ses = TRUE))
    })
    expect_true(test_against_margins(res, mar, tolerance = 0.1))
})

test_that("marginaleffects: vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))[["betareg_betareg_01"]]
    dat <- read.csv(test_path("stata/databases/betareg_betareg_01.csv"))
    mod <- betareg::betareg(yield ~ factor(batch) + temp, data = dat)
    mfx <- merge(tidy(marginaleffects(mod)), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)
})

test_that("predictions: no validity", {
    set.seed(1024)
    data("GasolineYield", package = "betareg")
    mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    pred <- predictions(mod)
    expect_predictions(pred, n_row = 1, n_col = 4, se = FALSE)
    pred <- predictions(mod, newdata = typical(batch = 1:3, temp = c(300, 350)))
    expect_predictions(pred, n_row = 6, n_col = 4, se = FALSE)
 })

test_that("marginalmeans: vs. emmeans", {
    set.seed(1024)
    data("GasolineYield", package = "betareg")
    mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    mm <- marginalmeans(mod)
    ti <- tidy(mm)
    em <- broom::tidy(emmeans::emmeans(mod, "batch"))
    expect_marginalmeans(mm, n_row = 10, n_col = 3, se = FALSE)
    expect_equal(ti$estimate, em$estimate)
 })
