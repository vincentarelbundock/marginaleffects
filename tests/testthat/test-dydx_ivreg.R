skip_if_not_installed("ivreg")

library("margins")

test_that("ivreg: vs. margins", {
    data(Kmenta, package = "ivreg")
    mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
    res <- mfx(mod)
    mar <- data.frame(margins(mod, unit_ses = TRUE))
    fastmargins:::test_against_margins(res, mar, tolerance = .01)
})    
