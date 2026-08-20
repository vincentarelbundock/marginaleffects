source(here::here("tests/r/load.R"))

dat <- fread(here("tests/data/quine.csv"), na.strings = c("NA", ""))
mod <- glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = dat)

predictions(mod, type = "response") |> 
    fwrite(here("tests/r/test_statsmodels_negativebinomial_predictions_01.csv"))

comparisons(mod, type = "response") |>
    fwrite(here("tests/r/test_statsmodels_negativebinomial_comparisons_01.csv"))
