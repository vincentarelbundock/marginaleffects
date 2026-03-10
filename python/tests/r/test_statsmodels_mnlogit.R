source(here::here("tests/r/load.R"))

dat = fread("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat = na.omit(dat, cols = c("species", "island", "bill_length_mm", "flipper_length_mm"))
dat[, island := factor(island, c("Torgersen", "Biscoe", "Dream"))]
mod = multinom(island ~ bill_length_mm + flipper_length_mm, data = dat, trace = FALSE)

predictions(mod, type = "probs") |>
    fwrite(here("tests/r/test_statsmodels_mnlogit_predictions_01.csv"))

predictions(mod, by = c("group", "species"), type = "probs") |>
    fwrite(here("tests/r/test_statsmodels_mnlogit_predictions_02.csv"))

comparisons(mod, type = "probs") |>
    fwrite(here("tests/r/test_statsmodels_mnlogit_comparisons_01.csv"))

comparisons(mod, by = c("group", "species"), type = "probs") |>
    fwrite(here("tests/r/test_statsmodels_mnlogit_comparisons_02.csv"))

# avg_predictions_01
dat = get_dataset("penguins", "palmerpenguins")
setDT(dat)
dat = na.omit(dat, cols = c("species", "island", "bill_length_mm", "flipper_length_mm"))
dat$island = factor(dat$island, levels = c("Biscoe", "Dream", "Torgersen"))
mod = multinom(island ~ bill_length_mm + flipper_length_mm, data = dat, trace = FALSE)
avg_predictions(mod) |>
    fwrite(here("tests/r/test_statsmodels_mnlogit_avg_predictions_01.csv"))
