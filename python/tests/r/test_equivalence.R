source(here::here("tests/r/load.R"))

# Read the same vendored CSV as the Python tests (tests/helpers.py) with the
# same missing-value rules: polars treats an empty field as null, so fread must
# use na.strings = c("NA", "") or the two sides disagree on which rows survive.
Guerry <- fread(here("tests/data/Guerry.csv"), na.strings = c("NA", ""))
Guerry <- na.omit(Guerry)
mod <- lm(Literacy ~ Pop1831 * Desertion, data = Guerry)

comparisons(mod, comparison = "differenceavg", equivalence = c(-.1, .1)) |>
    fwrite(here("tests/r/test_equivalence_01.csv"))
