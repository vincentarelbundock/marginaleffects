source(here::here("tests/r/load.R"))

Guerry <- fread("https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv")
Guerry <- na.omit(Guerry)
Guerry$rowid <- seq_len(nrow(Guerry))
setorder(Guerry, Region, rowid)
mod <- lm(Literacy ~ Pop1831 * Desertion, data = Guerry)

predictions(mod) |>
    fwrite("tests/r/test_predictions_01.csv")
predictions(mod, by = "Region") |>
    fwrite("tests/r/test_predictions_02.csv")
predictions(mod, by = "Region", hypothesis = "b1 * b3 = b3 * 2") |>
    fwrite("tests/r/test_predictions_03.csv")

predictions(mod, by = "Region")
