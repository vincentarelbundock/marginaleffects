source(here::here("tests/r/load.R"))

dat = transform(iris, bin = as.numeric(Sepal.Width < median(Sepal.Width)))
mod = glm(bin ~ Petal.Length * Petal.Width, data = dat, family = binomial(link = "probit"))

predictions(mod, type = "response") |> 
    fwrite(here("tests/r/test_statsmodels_probit_predictions_01.csv"))

predictions(mod, by = "Species", type = "response") |> 
    fwrite(here("tests/r/test_statsmodels_probit_predictions_02.csv"))

comparisons(mod, type = "response") |>
    fwrite(here("tests/r/test_statsmodels_probit_comparisons_01.csv"))

comparisons(mod, by = "Species", type = "response") |>
    fwrite(here("tests/r/test_statsmodels_probit_comparisons_02.csv"))
