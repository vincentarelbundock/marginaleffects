skip_if_not_installed("ivreg")

library("margins")
library("haven")
library("dplyr", warn.conflicts = FALSE)
library("ivreg")

test_that("ivreg vs. margins", {
    data(Kmenta, package = "ivreg")
    mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
    res <- meffects(mod)
    mar <- data.frame(margins(mod, unit_ses = TRUE))
    expect_true(test_against_margins(res, mar, tolerance = .01))
})    

test_that("ivreg vs. Stata", {
    dat <- read_dta(test_path("stata/data/ivreg_ivreg_01.dta"))
    stata <- readRDS(test_path("stata/stata.rds"))[["ivreg_ivreg_01"]]
    mod <- ivreg::ivreg(Q ~ P + D | D + F + A, data = dat)
    ame <- meffects(mod) %>%
           group_by(term) %>%
           summarize(dydx = mean(dydx),
                     std.error = mean(std.error)) %>%
           inner_join(stata, by = "term")
    expect_equal(ame$dydx, ame$dydxstata, tolerance = 0.0001)
})
