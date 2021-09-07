skip_if_not_installed("ivreg")

library("margins")

test_that("ivreg: vs. margins", {
    data(Kmenta, package = "ivreg")
    mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
    res <- marginsxp(mod)
    mar <- data.frame(margins(mod, unit_ses = TRUE))
    marginsxp:::test_against_margins(res, mar, tolerance = .01)
})    
