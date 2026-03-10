source(here::here("tests/r/load.R"))

dat = fread("https://vincentarelbundock.github.io/Rdatasets/csv/geepack/dietox.csv")
mod = lmer(Weight ~ Time * Litter + (1 | Pig), data = dat)

predictions(mod, type = "response", re.form = NA) |> 
    fwrite(here("tests/r/test_statsmodels_mixedlm_predictions_01.csv"))

predictions(mod, by = "Cu", type = "response", re.form = NA) |> 
    fwrite(here("tests/r/test_statsmodels_mixedlm_predictions_02.csv"))

comparisons(mod, type = "response", re.form = NA) |>
    fwrite(here("tests/r/test_statsmodels_mixedlm_comparisons_01.csv"))

comparisons(mod, by = "Cu", type = "response", re.form = NA) |>
    fwrite(here("tests/r/test_statsmodels_mixedlm_comparisons_02.csv"))
