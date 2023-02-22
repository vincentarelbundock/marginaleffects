source("helpers.R")
using("marginaleffects")

requiet("fixest")

# predictions() call is nested in a function
test <- function() {
  data(mtcars)
  test_data <- mtcars
  mod <- feols(mpg ~ hp + factor(cyl), data = test_data)
  nd <- datagrid(cyl = mtcars$cyl, newdata = test_data)
  preds <- predictions(mod, newdata = nd)
  return(preds)
}
p <- test()
expect_inherits(p, "predictions")
expect_equivalent(nrow(p), 3)



# yet another
test <- function() {
  data(mtcars)
  test_data <- mtcars
  mod <- feols(mpg ~ hp + factor(cyl), data = test_data)
  p <- predictions(mod, variables = "cyl", newdata = test_data)
  return(p)
}
p <- test()
expect_inherits(p, "predictions")
expect_equivalent(nrow(p), 96)


rm(list = ls())