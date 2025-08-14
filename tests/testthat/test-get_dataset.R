skip_if_offline()

test_that("get_dataset works with marginaleffects datasets", {
    dataset <- get_dataset("thornton")
    expect_s3_class(dataset, "data.frame")
    expect_gt(nrow(dataset), 0)
    expect_gt(ncol(dataset), 0)
})


test_that("get_dataset works with explicit package specification", {
    dataset <- get_dataset("thornton")
    dataset2 <- get_dataset("thornton", package = "marginaleffects")
    expect_s3_class(dataset2, "data.frame")
    expect_identical(dataset, dataset2)
})


test_that("get_dataset works with different datasets", {
    dat <- get_dataset("affairs")
    expect_s3_class(dat, "data.frame")
    expect_equal(nrow(dat), 601)

    dat <- get_dataset("airbnb")
    expect_s3_class(dat, "data.frame")
    expect_gt(nrow(dat), 50000)

    dat <- get_dataset("Titanic", "Stat2Data")
    expect_s3_class(dat, "data.frame")
    expect_equal(dim(dat), c(1313, 7))
})


test_that("get_dataset search functionality works", {
    search_results <- get_dataset(search = "iris")
    expect_s3_class(search_results, "data.frame")
    expect_true(all(c("iris", "iris3") %in% search_results$Item))
    expect_true(all("datasets" == search_results$Package))
})


test_that("get_dataset auto-detects package for unique dataset names", {
    iris_auto <- get_dataset("iris")
    expect_s3_class(iris_auto, "data.frame")
    expect_equal(nrow(iris_auto), 150)
})


test_that("get_dataset handles errors appropriately", {
    expect_error(get_dataset("nonexistent_dataset_xyz123"))
    expect_error(get_dataset("nonexistent", package = "marginaleffects"))
})
