source("helpers.R")

# error: supported model classes
model <- mtcars
class(model) <- "junk"
expect_error(marginaleffects(model), pattern = "not supported")
