source(here::here("tests/r/load.R"))

dat = fread("https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv")
mod <- lm(Literacy ~ Pop1831 * Desertion, data = dat)

hyp <- hypotheses(mod, hypothesis = c(1, -1, 0, 0))
fwrite(hyp, here("tests/r/test_hypotheses_coefs.csv"))

hyp <- comparisons(mod, by = TRUE, hypothesis = "b1 = b2")
fwrite(hyp, here("tests/r/test_hypotheses_comparisons.csv"))