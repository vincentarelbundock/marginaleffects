skip_if_not_installed("glmtoolbox")
withr_library("glmtoolbox")

test_that("glmtoolbox package works", {
    data(spruces)
    mod <- size ~ poly(days, 4) + treat
    fit <- glmgee(mod, id = tree, family = Gamma(log), corstr = "AR-M-dependent(1)", data = spruces)

    s <- avg_slopes(fit)
    expect_s3_class(s, "slopes")
    p <- avg_predictions(fit, by = "treat")
    expect_s3_class(p, "predictions")
    k <- avg_comparisons(fit)
    expect_s3_class(k, "comparisons")
})

