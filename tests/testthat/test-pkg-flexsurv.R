skip_if_not_installed("flexsurv")
withr_library("flexsurv")

test_that("flexsurv package works", {
    mod <- flexsurvreg(formula = Surv(futime, fustat) ~ age + ecog.ps, data = ovarian, dist = "gengamma")
    x <- avg_slopes(mod)
    expect_s3_class(x, "slopes")
    x <- predictions(mod)
    expect_s3_class(x, "predictions")
    x <- comparisons(mod)
    expect_s3_class(x, "comparisons")
})
