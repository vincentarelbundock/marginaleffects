skip_if_not_installed("fixest")

test_that("predictions() works when nested in a function with datagrid", {
    withr_library("fixest")
    test <- function() {
        data(mtcars)
        test_data <- mtcars
        mod <- feols(mpg ~ hp + factor(cyl), data = test_data)
        nd <- datagrid(cyl = mtcars$cyl, newdata = test_data)
        preds <- predictions(mod, newdata = nd, vcov = FALSE)
        return(preds)
    }
    p <- test()
    expect_s3_class(p, "predictions")
    expect_equal(nrow(p), 3)
})


test_that("predictions() works when nested in a function with variables", {
    test <- function() {
        data(mtcars)
        test_data <- mtcars
        mod <- feols(mpg ~ hp + factor(cyl), data = test_data)
        p <- predictions(mod, variables = "cyl", newdata = test_data, vcov = FALSE)
        return(p)
    }
    p <- test()
    expect_s3_class(p, "predictions")
    expect_equal(nrow(p), 96)
})
