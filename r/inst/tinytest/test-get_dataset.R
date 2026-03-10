source("helpers.R")

# Test marginaleffects dataset
dataset <- get_dataset("thornton")
expect_inherits(dataset, "data.frame")
expect_true(nrow(dataset) > 0)
expect_true(ncol(dataset) > 0)

# Test marginaleffects dataset with explicit package
dataset2 <- get_dataset("thornton", package = "marginaleffects")
expect_inherits(dataset2, "data.frame")
expect_identical(dataset, dataset2)

# Test different marginaleffects datasets
dat <- get_dataset("affairs")
expect_inherits(dat, "data.frame")
expect_equal(nrow(dat), 601)
dat <- get_dataset("airbnb")
expect_inherits(dat, "data.frame")
expect_true(nrow(dat) > 50000)
dat <- get_dataset("Titanic", "Stat2Data")
expect_inherits(dat, "data.frame")
expect_equal(dim(dat), c(1313, 7))

# Test search functionality if Rdatasets is available
# Test search function
search_results <- get_dataset(search = "iris")
expect_inherits(search_results, "data.frame")
expect_true(all(c("iris", "iris3") %in% search_results$Item))
expect_true(all("datasets" == search_results$Package))

# Test auto-detection of package for unique dataset names
iris_auto <- get_dataset("iris")
expect_inherits(iris_auto, "data.frame")
expect_equal(nrow(iris_auto), 150)

# Test error handling
expect_error(get_dataset("nonexistent_dataset_xyz123"))

# Test marginaleffects dataset that doesn't exist
expect_error(get_dataset("nonexistent", package = "marginaleffects"))

