source("helpers.R")
if (!EXPENSIVE) exit_file("EXPENSIVE")

set.seed(1024)
R <- 100
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

# simulation-based inference
x <- mod |> avg_predictions() |> inferences(method = "simulation", R = R)
expect_inherits(x, "predictions")
x <- mod |> slopes() |> inferences(method = "simulation", R = R) |> head()
expect_inherits(x, "slopes")
x <- mod |> predictions(vcov = "HC3") |> inferences(method = "simulation", R = R) |> head()
expect_inherits(x, "predictions")
x <- mod |> comparisons() |> inferences(method = "simulation", R = R) |> attr("posterior_draws")
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
x <- mod|> avg_comparisons() |> inferences(method = "simulation", R = R)
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

# fwb no validity check
x <- mod |> 
     comparisons() |> 
     inferences(method = "fwb", R = R)
expect_equivalent(nrow(x), 300)
x <- mod |> 
     avg_comparisons() |> 
     inferences(method = "fwb", R = R)
expect_equivalent(nrow(x), 2)


# {fwb} error when user supplied its own weightso
dat <- transform(mtcars, w = runif(32))
mod <- lm(mpg ~ hp, data = dat)
expect_error(inferences(comparisons(mod, wts = "w"), method = "fwb"), pattern = "wts")

# marginal_means not supported
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width * Species, data = iris)
expect_error(inferences(marginal_means(mod), method = "fwb"), pattern = "not supported")


rm(list = ls())