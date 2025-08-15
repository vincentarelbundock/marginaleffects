source("helpers.R")
if (!EXPENSIVE) exit_file("EXPENSIVE")
using("marginaleffects")

# inferences() currently returns a `comparisons` object even with `slopes()`

set.seed(1024)
R <- 25
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

set.seed(1234)
# {boot}
x <- mod |>
    avg_predictions() |>
    inferences(method = "boot", R = R)
expect_inherits(x, "predictions")
expect_equivalent(nrow(x), 1)

# head works
set.seed(1234)
x <- mod |>
    slopes() |>
    inferences(method = "boot", R = R)
expect_inherits(head(x), "slopes")
expect_equivalent(nrow(x), 300)
expect_equivalent(nrow(head(x)), 6)

# avg_ works
set.seed(1234)
x <- mod |>
    avg_slopes() |>
    inferences(method = "boot", R = R)
expect_inherits(x, "slopes") # should be slopes
expect_equivalent(nrow(x), 2)

x <- mod |>
    predictions(vcov = "HC3") |>
    inferences(method = "boot", R = R) |>
    head()
expect_inherits(x, "predictions")
x <- mod |>
    comparisons() |>
    inferences(method = "boot", R = R) |>
    attr("mfx")
expect_inherits(x@inferences, "boot")
nd <<- datagrid(Sepal.Length = range, model = mod)
x <- mod |>
    comparisons(variables = "Sepal.Width", newdata = nd) |>
    inferences(method = "boot", R = R)
expect_equivalent(nrow(x), 2)

# fwb no validity check
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

# {fwb} error when user supplied its own weights
dat <- transform(mtcars, w = runif(32))
mod <- lm(mpg ~ hp, data = dat)
expect_error(inferences(comparisons(mod, wts = "w"), method = "fwb"), pattern = "wts")

# Issue #856
tmp <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
cmp <- avg_comparisons(tmp, variables = list(Sepal.Length = 1, Species = "reference"), cross = TRUE) |>
    inferences(method = "boot", R = 5) |>
    suppressWarnings()
expect_inherits(cmp, "comparisons")
expect_equal(nrow(cmp), 2)

# Issue #853
m <- glm(am ~ mpg + hp + cyl, data = mtcars, family = binomial)
p <- avg_predictions(m, by = "cyl") |>
    inferences(method = "boot", R = 5) |>
    suppressWarnings()
expect_inherits(p, "predictions")
p <- predictions(m, by = "cyl") |>
    inferences(method = "boot", R = 5) |>
    suppressWarnings()
expect_inherits(p, "predictions")

# inferences with hypotheses
mod <- lm(mpg ~ hp + cyl, data = mtcars)
p <- hypotheses(mod, hypothesis = "hp/cyl=1") |>
    inferences(method = "boot", R = 25) |>
    suppressWarnings()
expect_inherits(p, "hypotheses")