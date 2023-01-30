source("helpers.R")
exit_if_not(packageVersion("base") >= "4.1.0")
exit_if_not(EXPENSIVE)

set.seed(1024)
R <- 1000
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)


# simulation-based inference
x <- mod |> avg_predictions() |> inferences(R = R)
expect_inherits(x, "predictions")
x <- mod |> slopes() |> inferences(R = R) |> head()
expect_inherits(x, "slopes")
x <- mod |> predictions(vcov = "HC3") |> inferences(R = R) |> head()
expect_inherits(x, "predictions")
x <- mod |> comparisons() |> inferences(R = R) |> attr("posterior_draws")
expect_inherits(x, "matrix")


# {boot}
x <- mod |> avg_predictions() |> inferences(method = "boot", R = R)
expect_inherits(x, "predictions")
expect_equivalent(nrow(x), 1)


# head works
x <- mod |> slopes() |> inferences(method = "boot", R = R)
expect_inherits(head(x), "slopes")
expect_equivalent(nrow(x), 300)
expect_equivalent(nrow(head(x)), 6)


# avg_ works
x <- mod |> avg_slopes() |> inferences(method = "boot", R = R)
expect_inherits(x, "slopes")
expect_equivalent(nrow(x), 2)


x <- mod |> predictions(vcov = "HC3") |> inferences(method = "boot", R = R) |> head()
expect_inherits(x, "predictions")
x <- mod |> comparisons() |> inferences(method = "boot", R = R) |> attr("inferences")
expect_inherits(x, "boot")
x <- mod |>
     comparisons(variables = "Sepal.Width", newdata = datagrid(Sepal.Length = range)) |> 
     inferences(method = "boot", R = R)
expect_equivalent(nrow(x), 2)
x <- mod|> avg_comparisons() |> inferences(R = R)
expect_equivalent(nrow(x), 2)
x <- x |> posterior_draws()
expect_equivalent(nrow(x), 2 * R)


# {rsample}
x <- mod |> avg_predictions() |> inferences(method = "rsample", R = R)
expect_inherits(x, "predictions")
x <- mod |> slopes() |> inferences(method = "rsample", R = R) |> head()
expect_inherits(x, "slopes")
x <- mod |> predictions(vcov = "HC3") |> inferences(method = "rsample", R = R) |> head()
expect_inherits(x, "predictions")
x <- mod |> comparisons() |> inferences(method = "rsample", R = R) |> attr("inferences")
expect_inherits(x, "bootstraps")
x <- mod |>
     comparisons(variables = "Sepal.Width", newdata = datagrid(Sepal.Length = range)) |>
     inferences(method = "rsample", R = R)
expect_equivalent(nrow(x), 2)
x <- mod |>
     avg_comparisons() |>
     inferences(method = "rsample", R = R) |>
     posterior_draws()
expect_equivalent(nrow(x), 2 * R)