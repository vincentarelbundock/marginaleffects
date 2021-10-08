skip_if_not_installed("ivreg")
requiet("margins")
requiet("dplyr")
requiet("ivreg")

test_that("marginaleffects: vs. margins", {
    data(Kmenta, package = "ivreg")
    mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
    res <- marginaleffects(mod)
    mar <- data.frame(margins(mod, unit_ses = TRUE))
    expect_true(test_against_margins(res, mar, tolerance = .1, verbose = TRUE))
})

test_that("marginaleffects: vs. Stata", {
    dat <- read.csv(test_path("stata/databases/ivreg_ivreg_01.csv"))
    stata <- readRDS(test_path("stata/stata.rds"))[["ivreg_ivreg_01"]]
    mod <- ivreg::ivreg(Q ~ P + D | D + F + A, data = dat)
    ame <- marginaleffects(mod) %>%
           group_by(term) %>%
           summarize(dydx = mean(dydx),
                     std.error = mean(std.error)) %>%
           inner_join(stata, by = "term")
    expect_equal(ame$dydx, ame$dydxstata, tolerance = 0.0001)
})

test_that("predictions: no validity", {
    data(Kmenta, package = "ivreg")
    mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
    pred1 <- predictions(mod)
    pred2 <- predictions(mod, newdata = head(Kmenta))
    expect_predictions(pred1, n_row = 1, se = FALSE)
    expect_predictions(pred2, n_row = 6, se = FALSE)
})

test_that("marginalmeans: no validity", {
    set.seed(1024)
    data(Kmenta, package = "ivreg")
    tmp <- Kmenta
    tmp$categ <- factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
    mod <- ivreg::ivreg(Q ~ P + D + categ | D + F + A + categ, data = tmp)
    mm <- marginalmeans(mod)
    expect_marginalmeans(mm)
})
