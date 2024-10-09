library(atime)   

test.list <- atime::atime_test_list(
  N = c(10, 100, 1000),
  seconds.limit = 3,
  setup = {
    dat <- data.frame(matrix(rnorm(N * 26), ncol = 26))
    mod <- lm(X1 ~ ., dat)
  },
  tests = list(
    # marginal effects at the mean; no standard error
    slopes_no_vcov_mean = slopes(mod, vcov = FALSE, newdata = "mean"),
    # marginal effects at the mean
    slopes_vcov_mean = slopes(mod, newdata = "mean"),
    # 1 variable; no standard error
    slopes_no_vcov_one_var = slopes(mod, vcov = FALSE, variables = "X3"),
    # 1 variable
    slopes_vcov_one_var = slopes(mod, variables = "X3"),
    # 26 variables; no standard error
    slopes_all_no_vcov = slopes(mod, vcov = FALSE),
    # 26 variables
    slopes_all_vcov = slopes(mod)
  )
)
