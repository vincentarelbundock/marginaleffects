source("helpers.R")
using("marginaleffects")

# Issue #903
set.seed(123)
dat <- data.frame(
  outcome = rbinom(n = 100, size = 1, prob = 0.35),
  var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
  var_cont = rnorm(n = 100, mean = 10, sd = 7),
  groups = sample(letters[1:4], size = 100, replace = TRUE)
)
m <- glm(outcome ~ var_binom * var_cont + groups,
  data = dat, family = binomial()
)

d <- structure(list(var_binom = structure(1:2, levels = c("0", "1"
), class = "factor"), var_cont = c(9.24717241397544, 9.24717241397544
), groups = structure(c(1L, 1L), levels = "b", class = "factor")), class = "data.frame", row.names = c(NA, 
-2L))

p <- predictions(
  m, newdata = d, by = "var_binom", hypothesis = "pairwise", transform = "exp", type = "response"
)
expect_equal(p$estimate, 0.9867827, tolerance = 0.0001)
p <- predictions(
  m, newdata = d, by = "var_binom", hypothesis = "pairwise", transform = "exp"
)
expect_equal(p$estimate, 1.61968, tolerance = 0.0001)