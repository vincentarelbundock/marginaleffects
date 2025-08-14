test_that("error: supported model classes", {
    model <- mtcars
    class(model) <- "junk"
    expect_error(slopes(model), regexp = "not supported")
})
