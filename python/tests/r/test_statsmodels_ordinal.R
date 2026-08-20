source(here::here("tests/r/load.R"))

# Read the same vendored CSV as tests/test_statsmodels_ordinal.py. The level
# order is spelled out on both sides: a CSV carries no factor metadata, and the
# default lexical order would put ">10" first and scramble the group labels.
dat <- fread(
    here("tests/data/affairs.csv"),
    na.strings = c("NA", ""),
    colClasses = c(affairs = "character")
)
dat$affairs <- factor(
    dat$affairs,
    levels = c("0", "1", "2", "3", "4-10", ">10"),
    ordered = TRUE
)

mod = polr(affairs ~ children + yearsmarried + gender, data = dat, method = "logistic")

# predictions on datagrid
predictions(mod, type = "probs",
    newdata = datagrid(children = "yes", yearsmarried = 10, gender = "woman")) |>
    fwrite(here("tests/r/test_statsmodels_ordinal_predictions_01.csv"))

# avg_predictions
avg_predictions(mod, type = "probs") |>
    fwrite(here("tests/r/test_statsmodels_ordinal_avg_predictions_01.csv"))

# slopes on datagrid
slopes(mod, type = "probs",
    newdata = datagrid(children = "yes", yearsmarried = 10, gender = "woman")) |>
    fwrite(here("tests/r/test_statsmodels_ordinal_slopes_01.csv"))

# avg_slopes
avg_slopes(mod, type = "probs") |>
    fwrite(here("tests/r/test_statsmodels_ordinal_avg_slopes_01.csv"))
