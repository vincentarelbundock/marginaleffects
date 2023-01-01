source("helpers.R", local = TRUE)
using("marginaleffects")

# error: supported model classes
model <- mtcars
class(model) <- "junk"
expect_error(marginaleffects(model), pattern = "not supported")
