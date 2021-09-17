skip_if_not_installed("survey")

requiet("margins")

test_that("survey: marginaleffects vs. margins", {
    data("fpc", package = "survey")
    svyd <- survey::svydesign(weights=~weight, 
                              ids=~psuid, 
                              strata=~stratid, 
                              fpc=~Nh, 
                              variables=~x + nh, 
                              data=fpc, 
                              nest=TRUE)
    mod <- survey::svyglm(x ~ nh, design = svyd)
    res <- marginaleffects(mod)
    mar <- suppressMessages(data.frame(margins(mod, unit_ses = TRUE)))
    # TODO: what explains this mismatch?
    expect_equal(res$dydx, as.numeric(mar$dydx_nh))
    expect_equal(res$std.error, as.numeric(mar$SE_dydx_nh), tolerance = 0.0001)
})
