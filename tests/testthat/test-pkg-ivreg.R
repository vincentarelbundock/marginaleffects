test_that("ivreg package works", {
    skip_if_not_installed("margins")
    skip_if_not_installed("dplyr")
    skip_if_not_installed("ivreg")

    withr_library("margins")
    withr_library("dplyr")
    withr_library("ivreg")

    # marginaleffects: vs. margins
    data(Kmenta, package = "ivreg")
    mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
    res <- slopes(mod)
    mar <- data.frame(margins(mod, unit_ses = TRUE))
    # Note: expect_margins is a custom function, using basic comparison
    expect_true(all(abs(res$estimate - mar$dydx) < 0.1))

    # plot_predictions: bugs stay dead
    # broke when no conf.low available
    data(Kmenta, package = "ivreg")
    mod <- ivreg::ivreg(Q ~ P + D + I(D^2) | D + I(D^2) + F + A, data = Kmenta)
    p <- plot_predictions(mod, condition = "D")
    expect_s3_class(p, "ggplot")

    # marginaleffects: vs. Stata
    dat <- read.csv(test_path("stata/databases/ivreg_ivreg_01.csv"))
    stata <- readRDS(test_path("stata/stata.rds"))[["ivreg_ivreg_01"]]
    mod <- ivreg::ivreg(Q ~ P + D | D + F + A, data = dat)
    ame <- slopes(mod) |>
        dplyr::group_by(term) |>
        dplyr::summarize(estimate = mean(estimate), std.error = mean(std.error)) |>
        dplyr::inner_join(stata, by = "term")
    expect_equal(ame$estimate, ame$dydxstata, tolerance = 0.0001, ignore_attr = TRUE)

    # predictions: no validity
    data(Kmenta, package = "ivreg")
    mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
    pred1 <- predictions(mod)
    pred2 <- predictions(mod, newdata = head(Kmenta))
    expect_s3_class(pred1, "predictions")
    expect_equal(nrow(pred1), nrow(Kmenta), ignore_attr = TRUE)
    expect_s3_class(pred2, "predictions")
    expect_equal(nrow(pred2), 6, ignore_attr = TRUE)
})
