source("helpers.R")
using("marginaleffects")

# Check fwb package version
if (!requireNamespace("fwb", quietly = TRUE)) {
    exit_file("fwb not available")
}
if (packageVersion("fwb") <= "0.5.0") {
    exit_file("version floor")
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
expect_equivalent(nrow(x), 300)
expect_equal(x$std.error[1:3], c(0.0642131648304821, 0.0444891291752277, 0.0442572266844693))
x <- mod |>
    avg_comparisons() |>
    inferences(method = "fwb", R = R) |>
    suppressWarnings()
expect_equivalent(nrow(x), 2)

# default case (no `newdata`): random weights reweight the averaging, as before
mod <- lm(mpg ~ hp * wt + factor(cyl), mtcars)
set.seed(48103)
x <- suppressWarnings(inferences(avg_comparisons(mod, variables = "hp", by = "am"), method = "fwb", R = 20))
expect_equivalent(x$std.error, c(0.010135652354473135, 0.011897911186007120))
