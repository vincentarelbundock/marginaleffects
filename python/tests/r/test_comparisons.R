source(here::here("tests/r/load.R"))

Guerry <- fread("https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv")
Guerry <- na.omit(Guerry)

Guerry$Bool <- Guerry$Area > median(Guerry$Area)
Guerry$Bin <- Guerry$Distance > median(Guerry$Distance)
Guerry$Bin <- as.integer(Guerry$Bin)
Guerry$Char <- sample(c("a", "b", "c"), nrow(Guerry), replace = TRUE)

mod <- lm(Literacy ~ Pop1831 * Desertion, data = Guerry)

# test_difference()
comparisons(mod, comparison = "differenceavg") |>
    fwrite("tests/r/test_comparisons_01.csv")
comparisons(mod, comparison = "difference", eps = 1e-4) |>
    fwrite("tests/r/test_comparisons_02.csv")


# test_comparison_simple()
estimands <- c("difference", "differenceavg", "differenceavgwts", "dydx", "eyex", "eydx", "dyex", "dydxavg", "eyexavg", "eydxavg", "dyexavg", "dydxavgwts", "eyexavgwts", "eydxavgwts", "dyexavgwts", "ratio", "ratioavg", "ratioavgwts", "lnratio", "lnratioavg", "lnratioavgwts", "lnor", "lnoravg", "lnoravgwts", "lift", "liftavg", "expdydx", "expdydxavg", "expdydxavgwts")
estimands <- estimands[!grepl("x|wts", estimands)]
for (e in estimands) {
    comparisons(mod, comparison = e, newdata = Guerry) |>
        fwrite(paste0("tests/r/test_comparisons_03_", e, ".csv"))
}


# test_by()
comparisons(mod, comparison = "differenceavg", by = "Region") |>
    fwrite("tests/r/test_comparisons_04.csv")


# test_HC3()
comparisons(mod, comparison = "differenceavg", vcov = "HC3") |>
    fwrite("tests/r/test_comparisons_05.csv")


# test_difference_wts()
comparisons(mod, variables = "Desertion", by = "Region", wts = "Literacy") |>
    fwrite("tests/r/test_comparisons_06.csv")
comparisons(mod, variables = "Desertion", by = "Region") |>
    fwrite("tests/r/test_comparisons_07.csv")
