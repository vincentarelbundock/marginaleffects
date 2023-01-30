source("helpers.R")
exit_if_not(packageVersion("base") >= "4.1.0")

set.seed(1024)
R <- 50

mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

# simulation-based inference
x <- mod |> inferences(R = R) |> avg_predictions()
expect_inherits(x, "predictions")
x <- mod |> inferences(R = R) |> slopes() |> head()
expect_inherits(x, "slopes")
x <- mod |> inferences(R = R) |> predictions(vcov = "HC3") |> head()
expect_inherits(x, "predictions")
x <- mod |> inferences(R = R) |> comparisons() |> attr("posterior_draws")
expect_inherits(x, "matrix")


# {boot}
x <- mod |> inferences(method = "boot", R = R) |> avg_predictions()
expect_inherits(x, "predictions")
x <- mod |> inferences(method = "boot", R = R) |> slopes() |> head()
expect_inherits(x, "slopes")
x <- mod |> inferences(method = "boot", R = R) |> predictions(vcov = "HC3") |> head()
expect_inherits(x, "predictions")
x <- mod |> inferences(method = "boot", R = R) |> comparisons() |> attr("boot")
expect_inherits(x, "boot")
x <- mod |> inferences(method = "boot", R = R) |> comparisons(variables = "Sepal.Width", newdata = datagrid(Sepal.Length = range))
expect_equivalent(nrow(x), 2)
x <- mod |> inferences(R = R) |> avg_comparisons() |> posterior_draws()
expect_equivalent(nrow(x), 2 * R)


# {rsample}
x <- mod |> inferences(method = "rsample", R = R) |> avg_predictions()
expect_inherits(x, "predictions")
x <- mod |> inferences(method = "rsample", R = R) |> slopes() |> head()
expect_inherits(x, "slopes")
x <- mod |> inferences(method = "rsample", R = R) |> predictions(vcov = "HC3") |> head()
expect_inherits(x, "predictions")
x <- mod |> inferences(method = "rsample", R = R) |> comparisons() |> attr("rsample")
expect_inherits(x, "bootstraps")
x <- mod |> inferences(method = "rsample", R = R) |> comparisons(variables = "Sepal.Width", newdata = datagrid(Sepal.Length = range))
expect_equivalent(nrow(x), 2)
x <- mod |> inferences(method = "rsample", R = R) |> avg_comparisons() |> posterior_draws()
expect_equivalent(nrow(x), 2 * R)