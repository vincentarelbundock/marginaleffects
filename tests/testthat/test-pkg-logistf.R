skip("logistf conflicts with other packages")

test_that("logistf package works", {
    skip_if_not_installed("logistf")
    skip_on_cran() # logistf causes conflicts

    withr_library("logistf")

    # logistf: no validity
    mod <- logistf(am ~ mpg * vs, data = mtcars)
    slo <- avg_slopes(mod)
    expect_s3_class(slo, "slopes")
    cmp <- avg_comparisons(mod, variables = list(mpg = "sd"))
    expect_s3_class(cmp, "comparisons")
    pre <- predictions(mod)
    expect_s3_class(pre, "predictions")

    # flic: no validity
    mod <- flic(am ~ mpg * vs, data = mtcars)
    slo <- avg_slopes(mod)
    expect_s3_class(slo, "slopes")
    cmp <- avg_comparisons(mod, variables = list(mpg = "sd"))
    expect_s3_class(cmp, "comparisons")
    pre <- predictions(mod)
    expect_s3_class(pre, "predictions")

    # flac: no validity
    mod <- flac(am ~ mpg * vs, data = mtcars)
    slo <- avg_slopes(mod)
    expect_s3_class(slo, "slopes")
    cmp <- avg_comparisons(mod, variables = list(mpg = "sd"))
    expect_s3_class(cmp, "comparisons")
    pre <- predictions(mod)
    expect_s3_class(pre, "predictions")
})
