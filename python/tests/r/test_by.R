source(here::here("tests/r/load.R"))

Guerry <- fread("https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv")
Guerry <- na.omit(Guerry)
mod <- lm(Literacy ~ Pop1831 * Desertion, data = Guerry)

predictions(mod, by = "Region") |>
    fwrite("tests/r/test_by_01.csv")
comparisons(mod, by = TRUE) |>
    fwrite("tests/r/test_by_02.csv")
comparisons(mod, by = FALSE) |>
    fwrite("tests/r/test_by_03.csv")
predictions(mod, by = "Region", wts = "Donations") |>
    fwrite("tests/r/test_by_04.csv")