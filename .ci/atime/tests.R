library(atime)

test.list <- atime::atime_test_list(
  N = c(10, 100, 1000),
  seconds.limit = 3,
  setup = {
    dat <- data.frame(matrix(rnorm(N * 26), ncol = 26))
    mod <- lm(X1 ~ ., dat)
  },
  # marginal effects at the mean; no standard error
  test1 = atime::atime_test(
    expr = marginaleffects::slopes(mod, vcov = FALSE, newdata = "mean")
  ),
  # marginal effects at the mean
  test2 = atime::atime_test(slopes(mod, newdata = "mean")),
  # 1 variable; no standard error
  test3 = atime::atime_test(slopes(mod, vcov = FALSE, variables = "X3")),
  # 1 variable
  test4 = atime::atime_test(slopes(mod, variables = "X3")),
  # 26 variables; no standard error
  test5 = atime::atime_test(slopes(mod, vcov = FALSE)),
  # 26 variables
  test6 = atime::atime_test(slopes(mod))
)
