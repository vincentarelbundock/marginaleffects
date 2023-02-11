source("helpers.R")
using("marginaleffects")

# error: supported model classes
model <- mtcars
class(model) <- "junk"
expect_error(slopes(model), pattern = "not supported")



rm(list = ls())