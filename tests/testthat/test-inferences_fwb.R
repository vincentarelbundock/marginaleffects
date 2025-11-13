testthat::skip_if(!EXPENSIVE, "EXPENSIVE")

testthat::skip_if_not_installed("fwb")

# Check fwb package version
if (packageVersion("fwb") <= "0.5.0") {
    testthat::skip("version floor")
}

# fwb no validity check
set.seed(1024)
R <- 25
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

set.seed(1234)
x <- mod |>
    comparisons() |>
    inferences(method = "fwb", R = R) |>
    suppressWarnings()
expect_equal(nrow(x), 300, ignore_attr = TRUE)
expect_equal(x$std.error[1:3], c(0.0642131648304821, 0.0444891291752277, 0.0442572266844693))
x <- mod |>
    avg_comparisons() |>
    inferences(method = "fwb", R = R) |>
    suppressWarnings()
expect_equal(nrow(x), 2, ignore_attr = TRUE)

# {fwb} error when user supplied its own weights
dat <- transform(mtcars, w = runif(32))
mod <- lm(mpg ~ hp, data = dat)
expect_error(inferences(comparisons(mod, wts = "w"), method = "fwb"), regexp = "wts")
