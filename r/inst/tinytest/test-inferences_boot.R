source("helpers.R")
using("marginaleffects")

# inferences() currently returns a `comparisons` object even with `slopes()`

set.seed(1024)
R <- 25
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

# bootstrap intervals roughly match analytic ones
set.seed(2024)
ref <- avg_predictions(mod)
set.seed(2024)
boot_ci <- avg_predictions(mod) |>
    inferences(method = "boot", R = 250)
expect_equal(boot_ci$conf.low, ref$conf.low, tolerance = 0.05)
expect_equal(boot_ci$conf.high, ref$conf.high, tolerance = 0.05)

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
    components("inferences")
expect_inherits(x, "boot")
nd <<- datagrid(Sepal.Length = range, model = mod)
x <- mod |>
    comparisons(variables = "Sepal.Width", newdata = nd) |>
    inferences(method = "boot", R = R)
expect_equivalent(nrow(x), 2)


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


# Issue #1713: inferences(method = "boot") should not pick up a global `data`
# variable with a `group` column during refit. Bug triggered when the formula
# was stored in a variable and passed to nls(), because stats::update() stores
# the symbol `data` (not its value) in the refitted model call, and insight
# then looks up `data` in environment(formula) = R_GlobalEnv.
# The test writes to globalenv to mirror the user's script-level usage.
set.seed(0)
assign("data", data.frame(x = 1:50, y = 1:50, group = "group1"), envir = globalenv())
assign("df1713", data.frame(x = 1:50, y = 10 * exp(-0.05 * (1:50)) + stats::rnorm(50, 0, 1)), envir = globalenv())
assign("formula1713", y ~ a * exp(-r * x), envir = globalenv())
assign("mod1713", nls(get("formula1713", envir = globalenv()), get("df1713", envir = globalenv()), start = c(a = 5, r = 0.01)), envir = globalenv())
ci1713 <- predictions(get("mod1713", envir = globalenv()), newdata = get("df1713", envir = globalenv())) |>
    inferences(method = "boot", R = 5)
expect_inherits(ci1713, "data.frame")
expect_true("conf.low" %in% names(ci1713))
expect_true("conf.high" %in% names(ci1713))
rm("data", "df1713", "formula1713", "mod1713", envir = globalenv())
rm(ci1713)
