# bootstrap with factors and small 
library(boot)

pkgload::load_all()

model <- lm(Petal.Length ~ Petal.Width * Sepal.Width, data = iris)

model |>
    inferences(method = "boot", R = 10000) |>
    predictions(newdata = datagrid(am = unique, newdata = iris))

model |>
    inferences(method = "delta") |>
    predictions(newdata = datagrid(Petal.Width = range, newdata = iris))

model |>
    inferences(method = "simulation") |>
    predictions(newdata = datagrid(Petal.Width = range))
