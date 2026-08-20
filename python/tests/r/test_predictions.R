source(here::here("tests/r/load.R"))

# Read the same vendored CSV as the Python tests (tests/helpers.py) with the
# same missing-value rules: polars treats an empty field as null, so fread must
# use na.strings = c("NA", "") or the two sides disagree on which rows survive.
Guerry <- fread(here("tests/data/Guerry.csv"), na.strings = c("NA", ""))
Guerry <- na.omit(Guerry)
# Shuffle the rows so the comparison actually exercises row alignment. The
# helper column is `row_id`, not `rowid`: `rowid` is a marginaleffects output
# column, and a user column of that name silently overwrites it.
Guerry$row_id <- seq_len(nrow(Guerry))
setorder(Guerry, Region, row_id)
mod <- lm(Literacy ~ Pop1831 * Desertion, data = Guerry)

predictions(mod) |>
    fwrite(here("tests/r/test_predictions_01.csv"))
predictions(mod, by = "Region") |>
    fwrite(here("tests/r/test_predictions_02.csv"))
predictions(mod, by = "Region", hypothesis = "b1 * b3 = b3 * 2") |>
    fwrite(here("tests/r/test_predictions_03.csv"))

predictions(mod, by = "Region")
