source("helpers.R")
# logistf breaks other tests
exit_file("conflict")
requiet("logistf")


# logistf: no validity
mod <- logistf(am ~ mpg * vs, data = mtcars)
slo <- avg_slopes(mod)
expect_inherits(slo, "slopes")
cmp <- avg_comparisons(mod, variables = list(mpg = "sd"))
expect_inherits(cmp, "comparisons")
pre <- predictions(mod)
expect_inherits(pre, "predictions")

# flic: no validity
mod <- flic(am ~ mpg * vs, data = mtcars)
slo <- avg_slopes(mod)
expect_inherits(slo, "slopes")
cmp <- avg_comparisons(mod, variables = list(mpg = "sd"))
expect_inherits(cmp, "comparisons")
pre <- predictions(mod)
expect_inherits(pre, "predictions")

# flac: no validity
mod <- flac(am ~ mpg * vs, data = mtcars)
slo <- avg_slopes(mod)
expect_inherits(slo, "slopes")
cmp <- avg_comparisons(mod, variables = list(mpg = "sd"))
expect_inherits(cmp, "comparisons")
pre <- predictions(mod)
expect_inherits(pre, "predictions")

# important to avoid conflicts
detach("package:logistf", unload = TRUE)
