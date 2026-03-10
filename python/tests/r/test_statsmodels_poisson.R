source(here::here("tests/r/load.R"))

mod = glm(carb ~ mpg * qsec + factor(cyl), data = mtcars, family = poisson)

predictions(mod, type = "response") |> 
    fwrite(here("tests/r/test_statsmodels_poisson_predictions_01.csv"))

predictions(mod, by = "cyl", type = "response") |> 
    fwrite(here("tests/r/test_statsmodels_poisson_predictions_02.csv"))

comparisons(mod, type = "response") |>
    fwrite(here("tests/r/test_statsmodels_poisson_comparisons_01.csv"))

comparisons(mod, by = "cyl", type = "response") |>
    fwrite(here("tests/r/test_statsmodels_poisson_comparisons_02.csv"))
