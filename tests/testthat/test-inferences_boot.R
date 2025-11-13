testthat::skip_if(!EXPENSIVE, "EXPENSIVE")

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
expect_s3_class(x, "predictions")
expect_equal(nrow(x), 1, ignore_attr = TRUE)

# head works
set.seed(1234)
x <- mod |>
    slopes() |>
    inferences(method = "boot", R = R)
expect_s3_class(head(x), "slopes")
expect_equal(nrow(x), 300, ignore_attr = TRUE)
expect_equal(nrow(head(x)), 6, ignore_attr = TRUE)

# avg_ works
set.seed(1234)
x <- mod |>
    avg_slopes() |>
    inferences(method = "boot", R = R)
expect_s3_class(x, "slopes") # should be slopes
expect_equal(nrow(x), 2, ignore_attr = TRUE)

x <- mod |>
    predictions(vcov = "HC3") |>
    inferences(method = "boot", R = R) |>
    head()
expect_s3_class(x, "predictions")
x <- mod |>
    comparisons() |>
    inferences(method = "boot", R = R) |>
    components("inferences")
expect_s3_class(x, "boot")
nd <<- datagrid(Sepal.Length = range, model = mod)
x <- mod |>
    comparisons(variables = "Sepal.Width", newdata = nd) |>
    inferences(method = "boot", R = R)
expect_equal(nrow(x), 2, ignore_attr = TRUE)


# Issue #856
tmp <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
cmp <- avg_comparisons(tmp, variables = list(Sepal.Length = 1, Species = "reference"), cross = TRUE) |>
    inferences(method = "boot", R = 5) |>
    suppressWarnings()
expect_s3_class(cmp, "comparisons")
expect_equal(nrow(cmp), 2)

# Issue #853
m <- glm(am ~ mpg + hp + cyl, data = mtcars, family = binomial)
p <- avg_predictions(m, by = "cyl") |>
    inferences(method = "boot", R = 5) |>
    suppressWarnings()
expect_s3_class(p, "predictions")
p <- predictions(m, by = "cyl") |>
    inferences(method = "boot", R = 5) |>
    suppressWarnings()
expect_s3_class(p, "predictions")

# inferences with hypotheses
mod <- lm(mpg ~ hp + cyl, data = mtcars)
p <- hypotheses(mod, hypothesis = "hp/cyl=1") |>
    inferences(method = "boot", R = 25) |>
    suppressWarnings()
expect_s3_class(p, "hypotheses")
