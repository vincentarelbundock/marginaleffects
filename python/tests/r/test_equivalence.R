source(here::here("tests/r/load.R"))

Guerry <- fread("https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv")
Guerry <- na.omit(Guerry)
mod <- lm(Literacy ~ Pop1831 * Desertion, data = Guerry)

comparisons(mod, comparison = "differenceavg", equivalence = c(-.1, .1)) |>
    fwrite("tests/r/test_equivalence_01.csv")
