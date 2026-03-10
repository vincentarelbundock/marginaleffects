source(here::here("tests/r/load.R"))

mod = lm(Sepal.Width ~ Petal.Length * Petal.Width, weights = iris$Sepal.Length, data = dat)

predictions(mod, type = "response") |> 
    fwrite(here("tests/r/test_statsmodels_wls_predictions_01.csv"))

predictions(mod, by = "Species", type = "response") |> 
    fwrite(here("tests/r/test_statsmodels_wls_predictions_02.csv"))

comparisons(mod, type = "response") |>
    fwrite(here("tests/r/test_statsmodels_wls_comparisons_01.csv"))

comparisons(mod, by = "Species", type = "response") |>
    fwrite(here("tests/r/test_statsmodels_wls_comparisons_02.csv"))
