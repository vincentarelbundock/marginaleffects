source(here::here("tests/r/load.R"))
mod = lm(mpg ~ wt * hp, mtcars)
for (est in c("dydx", "dydxavg", "dyex", "dyexavg", "eydx", "eydxavg", "eyex", "eyexavg")) {
    comparisons(mod, comparison = est) |>
        fwrite(paste0("tests/r/test_slopes_01_", est, ".csv"))
}
