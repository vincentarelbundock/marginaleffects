# bug fix submitted for this version of insight
skip_if_not_installed("insight", minimum_version = "0.17.1")

requiet("betareg")
requiet("margins")
requiet("emmeans")
requiet("broom")

tol <- 0.01 # bad tolerance for some results

data("GasolineYield", package = "betareg")
tmp <- GasolineYield
tmp$batch <- factor(tmp$batch)
dat <<- tmp
mod <<- betareg::betareg(yield ~ batch + temp, data = dat)

test_that("marginaleffects: vs. margins vs. emmeans", {
    set.seed(1024)
    suppressWarnings({
        res <- marginaleffects(mod, variables = "temp")
        mar <- data.frame(margins::margins(mod, unit_ses = TRUE))
    })
    expect_true(test_against_margins(res, mar, tolerance = 0.1))
    # emtrends
    mfx <- marginaleffects(mod, newdata = datagrid(batch = 1), variables = "temp")
    em <- suppressWarnings(
        emtrends(mod, ~temp, "temp", at = list("batch" = tmp$batch[1])))
    em <- tidy(em)
    expect_equal(mfx$dydx, em$temp.trend, tolerance = .001)
    expect_equal(mfx$std.error, em$std.error, tolerance = .001)
})

test_that("marginaleffects: vs. Stata", {
    # stata does not include contrasts
    stata <<- readRDS(test_path("stata/stata.rds"))[["betareg_betareg_01"]]
    mfx <- merge(tidy(marginaleffects(mod)), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)
})

test_that("predictions: no validity", {
    pred <- suppressWarnings(predictions(mod))
    expect_predictions(pred, n_row = nrow(GasolineYield))
    pred <- predictions(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)))
    expect_predictions(pred, n_row = 6)
})

test_that("marginalmeans: vs. emmeans", {
    # TODO: Bad tolerance
    mm <- marginalmeans(mod)
    expect_marginalmeans(mm, n_row = 10)
    mm <- tidy(mm)
    em <- broom::tidy(emmeans::emmeans(mod, "batch"))
    expect_equal(mm$estimate, em$estimate)
    expect_equal(mm$std.error, em$std.error, tolerance = tol)
})
