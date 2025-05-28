library(collapse)
draws <- matrix(rnorm(16), ncol = 4)
labels <- letters[1:nrow(draws)]
estimates <- rowMeans(draws)
comparison <- "difference"
comparison <- "ratio"
by <- factor(c("a", "a", "b", "b"))

hypothesis_reference(estimates, labels, draws, by)
hypothesis_reference(estimates, labels, draws, by, reverse = TRUE)
