source(here::here("tests/r/load.R"))

# Read the same vendored CSV as the Python tests (tests/helpers.py) with the
# same missing-value rules: polars treats an empty field as null, so fread must
# use na.strings = c("NA", "") or the two sides disagree on which rows survive.
dat <- fread(here("tests/data/Guerry.csv"), na.strings = c("NA", ""))
mod <- lm(Literacy ~ Pop1831 * Desertion, data = dat)

hyp <- hypotheses(mod, hypothesis = c(1, -1, 0, 0))
fwrite(hyp, here("tests/r/test_hypotheses_coefs.csv"))

hyp <- comparisons(mod, by = TRUE, hypothesis = "b1 = b2")
fwrite(hyp, here("tests/r/test_hypotheses_comparisons.csv"))