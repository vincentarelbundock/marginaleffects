skip_if(getRversion() < 4.0)
skip_if_not_installed("ivreg")
requiet("margins")
requiet("dplyr")
requiet("ivreg")

test_that("ivreg vs. margins", {
    data(Kmenta, package = "ivreg")
    mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
    res <- marginaleffects(mod)
    mar <- data.frame(margins(mod, unit_ses = TRUE))
    expect_true(test_against_margins(res, mar, tolerance = .1, verbose = TRUE))
})    

test_that("ivreg vs. Stata", {
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
