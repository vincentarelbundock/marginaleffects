source("helpers.R")

# Test get_dataset function
dataset <- get_dataset("thornton")
expect_inherits(dataset, "data.frame")