skip_if_not_installed("survey")

test_that("survey: fastmargins vs. margins", {
    data("fpc", package = "survey")
    svyd <- survey::svydesign(weights=~weight, 
                              ids=~psuid, 
                              strata=~stratid, 
                              fpc=~Nh, 
                              variables=~x + nh, 
                              data=fpc, 
                              nest=TRUE)
    mod <- survey::svyglm(x ~ nh, design = svyd)
    res <- mfx(mod)
    mar <- data.frame(margins(mod, unit_ses = TRUE))
    expect_equal(res$dydx_nh, as.numeric(mar$dydx_nh))
    # TODO: what explains this mismatch?
    expect_equal(res$se_dydx_nh, as.numeric(mar$SE_dydx_nh), tolerance = 0.00001)
})
