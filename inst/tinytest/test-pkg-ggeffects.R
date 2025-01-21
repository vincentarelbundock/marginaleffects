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

p1 <- predictions(
  m, newdata = d, by = "var_binom", hypothesis = ~pairwise, type = "response", transform = exp
)
p2 <- predictions(
  m, newdata = d, by = "var_binom", hypothesis = ~pairwise, type = "invlink(link)", transform = exp
) |> suppressWarnings()
p3 <- predictions(
  m, newdata = d, by = "var_binom", hypothesis = ~pairwise, type = NULL, transform = exp
) |> suppressWarnings()

# values
expect_equal(p1$estimate[2], 0.9867827, tolerance = 1e-5)
expect_equal(p2$estimate[2], 0.9867827, tolerance = 1e-5)
expect_equal(p3$estimate[2], 0.9867827, tolerance = 1e-5)

