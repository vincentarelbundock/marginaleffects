source(here::here("tests/r/load.R"))

# Read the same vendored CSV as the Python tests (tests/helpers.py) with the
# same missing-value rules: polars treats an empty field as null, so fread must
# use na.strings = c("NA", "") or the two sides disagree on which rows survive.
Guerry <- fread(here("tests/data/Guerry.csv"), na.strings = c("NA", ""))
Guerry <- na.omit(Guerry)
mod <- lm(Literacy ~ Pop1831 * Desertion, data = Guerry)

predictions(mod, by = "Region") |>
    fwrite(here("tests/r/test_by_01.csv"))
comparisons(mod, by = TRUE) |>
    fwrite(here("tests/r/test_by_02.csv"))
comparisons(mod, by = FALSE) |>
    fwrite(here("tests/r/test_by_03.csv"))
predictions(mod, by = "Region", wts = "Donations") |>
    fwrite(here("tests/r/test_by_04.csv"))