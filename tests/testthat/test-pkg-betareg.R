requiet("betareg")
requiet("margins")
requiet("emmeans")
requiet("broom")

test_that("marginaleffects: vs. margins vs. emmeans", {
    set.seed(1024)
    data("GasolineYield", package = "betareg")
    tmp <- GasolineYield
    tmp$batch <- factor(tmp$batch)
    mod <- betareg::betareg(yield ~ batch + temp, data = tmp)
    suppressWarnings({
        res <- marginaleffects(mod, variables = "temp")
        mar <- data.frame(margins::margins(mod, unit_ses = TRUE))
    })
    expect_true(test_against_margins(res, mar, tolerance = 0.1))
    # emtrends
    mfx <- marginaleffects(mod, newdata = datagrid(batch = 1), variables = "temp")
    em <- suppressWarnings(
        emtrends(mod, ~temp, "temp", at = list("batch" = GasolineYield$batch[1])))
    em <- tidy(em)
    expect_equal(mfx$dydx, em$temp.trend, tolerance = .001)
    expect_equal(mfx$std.error, em$std.error, tolerance = .001)
})

test_that("marginaleffects: vs. Stata", {
    # stata does not include contrasts
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
    expect_predictions(pred, n_row = nrow(GasolineYield))
    pred <- predictions(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)))
    expect_predictions(pred, n_row = 6)
 })

test_that("marginalmeans: vs. emmeans", {
    # TODO: Bad tolerance
    set.seed(1024)
    data("GasolineYield", package = "betareg")
    mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)

    mm <- marginalmeans(mod)
    expect_marginalmeans(mm, n_row = 10)
    mm <- tidy(mm)
    em <- broom::tidy(emmeans::emmeans(mod, "batch"))
    expect_equal(mm$estimate, em$estimate)
    expect_equal(mm$std.error, em$std.error, tolerance = .01)
})
