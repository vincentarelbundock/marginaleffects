testthat::skip_if_not_installed("fixest")
requiet("fixest")

# predictions() call is nested in a function
test <- function() {
    data(mtcars)
    test_data <- mtcars
    mod <- fixest::feols(mpg ~ hp + factor(cyl), data = test_data)
    nd <- datagrid(cyl = mtcars$cyl, newdata = test_data)
    preds <- predictions(mod, newdata = nd, vcov = FALSE)
    return(preds)
}
p <- test()
expect_s3_class(p, "predictions")
expect_equal(nrow(p), 3, ignore_attr = TRUE)


# yet another
test <- function() {
    data(mtcars)
    test_data <- mtcars
    mod <- fixest::feols(mpg ~ hp + factor(cyl), data = test_data)
    p <- predictions(mod, variables = "cyl", newdata = test_data, vcov = FALSE)
    return(p)
}
p <- test()
expect_s3_class(p, "predictions")
expect_equal(nrow(p), 96, ignore_attr = TRUE)
