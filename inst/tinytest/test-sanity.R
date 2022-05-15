# error: supported model classes
model <- mtcars
class(model) <- "junk"
expect_error(marginaleffects(model), pattern = "not supported")



# dependency assertion
k <- check_dependency("blahblah")
expect_equivalent(k, "Please install the `blahblah` package.")

