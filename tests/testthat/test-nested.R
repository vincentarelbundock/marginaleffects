requiet("fixest")
 
test_that("predictions() call is nested in a function", {
    test <- function() {
      data(mtcars)
      test_data <- mtcars
      mod <- feols(mpg ~ hp + factor(cyl), data = test_data)
      nd <- datagrid(cyl = mtcars$cyl, newdata = test_data)
      preds <- predictions(mod, newdata = nd)
      return(preds)
    }
    p <- test()
    expect_s3_class(p, "predictions")
    expect_equal(nrow(p), 3)
})


test_that("informative error", {
    test <- function() {
        data(mtcars)
        test_data <- mtcars
        mod <- feols(mpg ~ hp + factor(cyl), data = test_data)
        return(mod)
    }
    expect_error(predictions(test(), variables = "cyl"), regexp = "explicitly")
})


test_that("yet another", {
    test <- function() {
        data(mtcars)
        test_data <- mtcars
        mod <- feols(mpg ~ hp + factor(cyl), data = test_data)
        p <- predictions(mod, variables = "cyl", newdata = test_data)
        return(p)
    }
    p <- test()
    expect_s3_class(p, "predictions")
    expect_equal(nrow(p), 3)
})
