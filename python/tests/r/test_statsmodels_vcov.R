source(here::here("tests/r/load.R"))
library(sandwich)

# Read the same vendored CSV as tests/test_statsmodels_vcov.py.
dat <- fread(here("tests/data/thornton.csv"), na.strings = c("NA", ""))
dat <- na.omit(dat, cols = c("outcome", "distance", "age"))

mod <- lm(outcome ~ distance + age, data = dat)

# predictions with HC3
predictions(mod, vcov = "HC3") |>
    fwrite(here("tests/r/test_statsmodels_vcov_predictions_hc3.csv"))

# avg_predictions with HC3
avg_predictions(mod, vcov = "HC3") |>
    fwrite(here("tests/r/test_statsmodels_vcov_avg_predictions_hc3.csv"))

# slopes with HC3
avg_slopes(mod, vcov = "HC3") |>
    fwrite(here("tests/r/test_statsmodels_vcov_avg_slopes_hc3.csv"))

# slopes with HC0, HC1, HC2
avg_slopes(mod, vcov = "HC0") |>
    fwrite(here("tests/r/test_statsmodels_vcov_avg_slopes_hc0.csv"))

avg_slopes(mod, vcov = "HC1") |>
    fwrite(here("tests/r/test_statsmodels_vcov_avg_slopes_hc1.csv"))

avg_slopes(mod, vcov = "HC2") |>
    fwrite(here("tests/r/test_statsmodels_vcov_avg_slopes_hc2.csv"))
