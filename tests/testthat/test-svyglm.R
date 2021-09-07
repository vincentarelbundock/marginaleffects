skip_if_not_installed("survey")

library("margins")

test_that("survey: meffects vs. margins", {
    data("fpc", package = "survey")
    svyd <- survey::svydesign(weights=~weight, 
                              ids=~psuid, 
                              strata=~stratid, 
                              fpc=~Nh, 
                              variables=~x + nh, 
                              data=fpc, 
                              nest=TRUE)
    mod <- survey::svyglm(x ~ nh, design = svyd)
    res <- meffects(mod)
    mar <- data.frame(margins(mod, unit_ses = TRUE))
    # TODO: what explains this mismatch?
    expect_equal(res$dydx, as.numeric(mar$dydx_nh))
    expect_equal(res$std.error, as.numeric(mar$SE_dydx_nh), tolerance = 0.0001)
})
