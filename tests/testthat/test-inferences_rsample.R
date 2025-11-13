testthat::skip_if(!EXPENSIVE, "EXPENSIVE")

set.seed(1024)
R <- 25
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

# rsample intervals roughly match analytic ones
set.seed(404)
ref <- avg_predictions(mod)
set.seed(404)
rs_ci <- avg_predictions(mod) |>
    inferences(method = "rsample", R = 250) |>
    suppressWarnings()
expect_equal(rs_ci$conf.low, ref$conf.low, tolerance = 0.05)
expect_equal(rs_ci$conf.high, ref$conf.high, tolerance = 0.05)

# {rsample}
set.seed(1234)
x <- mod |>
    avg_predictions() |>
    inferences(method = "rsample", R = R) |>
    suppressWarnings()
expect_equal(x$conf.low, 3.665, tolerance = 1e-3)
expect_s3_class(x, "predictions")
x <- mod |>
    slopes() |>
    inferences(method = "rsample", R = R) |>
    suppressWarnings()
expect_s3_class(x, "slopes")
x <- mod |>
    predictions(vcov = "HC3") |>
    inferences(method = "rsample", R = R) |>
    suppressWarnings()
expect_s3_class(x, "predictions")
x <- mod |>
    comparisons() |>
    inferences(method = "rsample", R = R) |>
    components("inferences") |>
    suppressWarnings()
expect_s3_class(x, "bootstraps")
nd <<- datagrid(Sepal.Length = range, model = mod)
x <- mod |>
    comparisons(variables = "Sepal.Width", newdata = nd) |>
    inferences(method = "rsample", R = R) |>
    suppressWarnings()
expect_equal(nrow(x), 2, ignore_attr = TRUE)
x <- mod |>
    avg_comparisons() |>
    inferences(method = "rsample", R = R) |>
    get_draws() |>
    suppressWarnings()
expect_equal(nrow(x), 2 * R, ignore_attr = TRUE)

# Bug: rsample collapses non-unique term
mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
k <- avg_comparisons(mod) |>
    inferences(method = "rsample", R = R) |>
    suppressWarnings()
expect_s3_class(k, "comparisons")

# `estimator` function
lalonde <- get_dataset("lalonde")
estimator <- function(data) {
    fit1 <- glm(treat ~ age + educ + race, family = binomial, data = data)
    ps <- predict(fit1, type = "response")
    m <- lm(re78 ~ treat * (re75 + age + educ + race), data = data, weight = ps)
    avg_comparisons(m, variables = "treat", wts = ps, vcov = FALSE)
}
cmp <- inferences(lalonde, method = "rsample", estimator = estimator, R = R) |>
    suppressWarnings()
expect_s3_class(cmp, "comparisons")
expect_error(inferences(lalonde, method = "rsample"), "when supplying a function to the `estimator` argument.")
expect_error(inferences(estimator(lalonde), estimator = estimator, method = "rsample"), "The `x` argument must be a raw data frame when using the `estimator` argument.")
suppressWarnings(inferences(lalonde, method = "rsample", estimator = estimator, R = 3))
expect_true(TRUE) # Test that previous line doesn't error

# survival vignette
testthat::skip_if_not_installed("survival")
testthat::skip_if_not_installed("splines")
requiet("survival")
requiet("splines")
model <- coxph(
    Surv(dtime, death) ~ hormon * factor(grade) + ns(age, df = 2),
    data = rotterdam
)
nd <<- datagrid(
    hormon = unique,
    grade = unique,
    dtime = seq(36, 7043, length.out = 25),
    grid_type = "counterfactual",
    model = model
)

p <- predictions(model, type = "survival", by = c("dtime", "hormon", "grade"), newdata = nd)
p <- inferences(p, method = "rsample", R = R) |> suppressWarnings()
expect_true(all(p$estimate >= p$conf.low))
expect_true(all(p$estimate <= p$conf.high))
