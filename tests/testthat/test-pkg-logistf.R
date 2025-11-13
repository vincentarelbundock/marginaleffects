testthat::skip("conflict - logistf breaks other tests")
testthat::skip_if_not_installed("logistf")
requiet("logistf")


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

# important to avoid conflicts
detach("package:logistf", unload = TRUE)
