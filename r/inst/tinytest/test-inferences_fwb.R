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
modw <- lm(mpg ~ hp * wt + factor(cyl), mtcars, weights = qsec)
set.seed(48103)
x <- suppressWarnings(inferences(avg_comparisons(modw, variables = "hp"), method = "fwb", R = 20))
expect_equivalent(x$std.error, 0.010547942262950505)

# `newdata` with other rows than the model data: model refit, averaging fixed
mod <- lm(mpg ~ hp * wt + factor(cyl), mtcars)
nd <- subset(mtcars, cyl == 4)
options(marginaleffects_fwb_newdata_fixed = TRUE, marginaleffects_safe = TRUE)
msgs <- character()
set.seed(1024)
x <- withCallingHandlers(
    inferences(avg_comparisons(mod, variables = "hp", newdata = nd), method = "fwb", R = 20),
    warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
options(marginaleffects_safe = FALSE)
expect_true(any(grepl("treated as fixed", msgs)))
expect_equivalent(x$estimate, avg_comparisons(mod, variables = "hp", newdata = nd)$estimate)
expect_true(all(is.finite(x$std.error)))
expect_true(all(x$std.error > 0))
x <- suppressWarnings(inferences(avg_comparisons(mod, variables = "hp", newdata = datagrid(cyl = c(4, 6))), method = "fwb", R = 20))
expect_true(all(is.finite(x$std.error)))

# user `wts` are honoured, over the model data and over another `newdata`
dat <- transform(mtcars, w = runif(32))
mod <- lm(mpg ~ hp * wt, data = dat)
ref <- avg_comparisons(mod, variables = "hp", wts = "w")
set.seed(1024)
x <- suppressWarnings(inferences(avg_comparisons(mod, variables = "hp", wts = "w"), method = "fwb", R = 20))
expect_equivalent(x$estimate, ref$estimate)
expect_true(all(is.finite(x$std.error)))
x <- suppressWarnings(inferences(avg_comparisons(mod, variables = "hp", wts = dat$w), method = "fwb", R = 20))
expect_equivalent(x$estimate, ref$estimate)
expect_true(all(is.finite(x$std.error)))
nd <- subset(dat, cyl == 4)
ref <- avg_comparisons(mod, variables = "hp", newdata = nd, wts = "w")
x <- suppressWarnings(inferences(avg_comparisons(mod, variables = "hp", newdata = nd, wts = "w"), method = "fwb", R = 20))
expect_equivalent(x$estimate, ref$estimate)
expect_true(all(is.finite(x$std.error)))