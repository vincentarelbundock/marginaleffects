source(here::here("python/tests/r/load.R"))

dat = get_dataset("affairs")
setDT(dat)
dat$affairs = factor(dat$affairs, ordered = TRUE)

mod = polr(affairs ~ children + yearsmarried + gender, data = dat, method = "logistic")

# predictions on datagrid
predictions(mod, type = "probs",
    newdata = datagrid(children = "yes", yearsmarried = 10, gender = "woman")) |>
    fwrite(here("python/tests/r/test_statsmodels_ordinal_predictions_01.csv"))

# avg_predictions
avg_predictions(mod, type = "probs") |>
    fwrite(here("python/tests/r/test_statsmodels_ordinal_avg_predictions_01.csv"))

# slopes on datagrid
slopes(mod, type = "probs",
    newdata = datagrid(children = "yes", yearsmarried = 10, gender = "woman")) |>
    fwrite(here("python/tests/r/test_statsmodels_ordinal_slopes_01.csv"))

# avg_slopes
avg_slopes(mod, type = "probs") |>
    fwrite(here("python/tests/r/test_statsmodels_ordinal_avg_slopes_01.csv"))
