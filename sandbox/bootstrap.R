# bootstrap with factors and small 
library(boot)

pkgload::load_all()

model <- lm(Petal.Length ~ Petal.Width * Sepal.Width, data = iris)

pkgload::load_all()

# predictions
model |>
    inferences(method = "delta") |>
    predictions(newdata = datagrid(Petal.Width = range))

p <- model |>
    inferences(method = "boot", sim = "balanced", R = 100) |>
    predictions(newdata = datagrid(Petal.Width = range))
p
attr(p, "boot") |> boot.ci(type = "perc")

model |>
    inferences(method = "simulation") |>
    predictions(newdata = datagrid(Petal.Width = range))


# comparisons
model |>
    inferences(method = "delta") |>
    comparisons(newdata = datagrid(Petal.Width = range))

model |>
    inferences(method = "boot", R = 1000) |>
    comparisons(newdata = datagrid(Petal.Width = range))

model |>
    inferences(method = "simulation") |>
    comparisons(newdata = datagrid(Petal.Width = range))


# slopes
model |>
    inferences(method = "delta") |>
    slopes(newdata = datagrid(Petal.Width = range))

model |>
    inferences(method = "boot") |>
    slopes(newdata = datagrid(Petal.Width = range))

model |>
    inferences(method = "simulation") |>
    slopes(newdata = datagrid(Petal.Width = range))
