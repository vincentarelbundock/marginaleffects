# requires marginaleffects >= 0.13.0.9002
source(here::here("tests/r/load.R"))

mod = rq(Sepal.Length ~ Sepal.Width * Petal.Length + Species, tau = .25, data = iris)

predictions(mod) |> 
    write.csv(here("tests/r/test_statsmodels_quantreg_predictions_01.csv"), na = "")

predictions(mod, by = "Species") |> 
    write.csv(here("tests/r/test_statsmodels_quantreg_predictions_02.csv"), na = "")

comparisons(mod) |>
    write.csv(here("tests/r/test_statsmodels_quantreg_comparisons_01.csv"), na = "")

comparisons(mod, by = "Species") |>
    write.csv(here("tests/r/test_statsmodels_quantreg_comparisons_02.csv"), na = "")
