test_that("error: supported model classes", {
    model <- mtcars
    class(model) <- "junk"
    expect_error(marginaleffects(model), regexp = "not supported")
})


test_that("dependency assertion", {
    k <- check_dependency("blahblah")
    expect_equal(k, "Please install the `blahblah` package.")
})
