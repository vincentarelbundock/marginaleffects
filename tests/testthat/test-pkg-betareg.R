test_that("betareg package works", {
    skip_if_not_installed("betareg")
    skip_if_not_installed("margins")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("betareg")
    withr_library("margins")
    withr_library("emmeans")
    withr_library("broom")

    data("GasolineYield", package = "betareg")
    tmp <- GasolineYield
    tmp$batch <- factor(tmp$batch)
    dat <- tmp
    mod <- betareg::betareg(yield ~ batch + temp, data = dat)

    # marginaleffects: vs. margins vs. emmeans
    set.seed(1024)
    res <- slopes(mod, variables = "temp")
    mar <- data.frame(margins::margins(mod, unit_ses = TRUE))
    expect_true(all(abs(res$estimate - mar$dydx) < 0.1))

    # emtrends
    mfx <- slopes(mod, newdata = datagrid(batch = 1), variables = "temp")
    em <- suppressWarnings(
        emtrends(mod, ~temp, "temp", at = list("batch" = tmp$batch[1]))
    )
    em <- tidy(em)
    expect_equal(mfx$estimate, em$temp.trend, tolerance = .001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)

    # marginaleffects: vs. Stata
    # stata does not include contrasts
    stata <- readRDS(test_path("stata/stata.rds"))[["betareg_betareg_01"]]
    mfx <- merge(avg_slopes(mod), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001, ignore_attr = TRUE)

    # predictions: no validity
    pred <- suppressWarnings(predictions(mod))
    expect_s3_class(pred, "predictions")
    expect_equal(nrow(pred), nrow(GasolineYield), ignore_attr = TRUE)
    pred <- predictions(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)))
    expect_s3_class(pred, "predictions")
    expect_equal(nrow(pred), 6, ignore_attr = TRUE)

    # link
    mm <- predictions(mod, type = "link", by = "batch", newdata = datagrid(grid_type = "balanced")) |>
        dplyr::arrange(batch)

    # marginalmeans: vs. emmeans
    mm <- predictions(mod, type = "response", by = "batch", newdata = datagrid(grid_type = "balanced")) |>
        dplyr::arrange(batch)
    expect_s3_class(mm, "predictions")
    expect_equal(nrow(mm), 10, ignore_attr = TRUE)
    em <- broom::tidy(emmeans::emmeans(mod, "batch"))
    expect_equal(mm$estimate, em$estimate, ignore_attr = TRUE)
    expect_equal(mm$std.error, em$std.error, tolerance = 0.01, ignore_attr = TRUE)

    # Issue #1391
    mm_link <- predictions(mod, type = "link", by = "batch", newdata = datagrid(grid_type = "balanced")) |>
        dplyr::arrange(batch)
    expect_true(all(mm_link$estimate < mm$estimate))
})
