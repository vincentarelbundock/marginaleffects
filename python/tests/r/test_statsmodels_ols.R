source(here::here("tests/r/load.R"))

dat = transform(mtcars, cyl = as.character(cyl))
mod = lm(mpg ~ qsec * wt + cyl, data = dat)

predictions(mod, type = "response") |> 
    fwrite(here("tests/r/test_statsmodels_ols_predictions_01.csv"))

predictions(mod, by = "carb", type = "response") |> 
    fwrite(here("tests/r/test_statsmodels_ols_predictions_02.csv"))

comparisons(mod, type = "response") |>
    fwrite(here("tests/r/test_statsmodels_ols_comparisons_01.csv"))

comparisons(mod, by = "carb", type = "response") |>
    fwrite(here("tests/r/test_statsmodels_ols_comparisons_02.csv"))

slopes(mod, type = "response") |>
    fwrite(here("tests/r/test_statsmodels_ols_slopes_01.csv"))

slopes(mod, by = "carb", type = "response") |>
    fwrite(here("tests/r/test_statsmodels_ols_slopes_02.csv"))