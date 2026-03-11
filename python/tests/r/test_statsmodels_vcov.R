source(here::here("python/tests/r/load.R"))
library(sandwich)

dat <- get_dataset("thornton")
setDT(dat)
dat <- na.omit(dat, cols = c("outcome", "distance", "age"))

mod <- lm(outcome ~ distance + age, data = dat)

# predictions with HC3
predictions(mod, vcov = "HC3") |>
    fwrite(here("python/tests/r/test_statsmodels_vcov_predictions_hc3.csv"))

# avg_predictions with HC3
avg_predictions(mod, vcov = "HC3") |>
    fwrite(here("python/tests/r/test_statsmodels_vcov_avg_predictions_hc3.csv"))

# slopes with HC3
avg_slopes(mod, vcov = "HC3") |>
    fwrite(here("python/tests/r/test_statsmodels_vcov_avg_slopes_hc3.csv"))

# slopes with HC0, HC1, HC2
avg_slopes(mod, vcov = "HC0") |>
    fwrite(here("python/tests/r/test_statsmodels_vcov_avg_slopes_hc0.csv"))

avg_slopes(mod, vcov = "HC1") |>
    fwrite(here("python/tests/r/test_statsmodels_vcov_avg_slopes_hc1.csv"))

avg_slopes(mod, vcov = "HC2") |>
    fwrite(here("python/tests/r/test_statsmodels_vcov_avg_slopes_hc2.csv"))
