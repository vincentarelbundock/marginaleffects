exit_file("fixest data in the environment")
source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
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



# informative error
test <- function() {
data(mtcars)
test_data <- mtcars
mod <- feols(mpg ~ hp + factor(cyl), data = test_data)
return(mod)
}
expect_error(predictions(test(), variables = "cyl"), pattern = "explicitly")



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
expect_equivalent(nrow(p), 3)

