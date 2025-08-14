test_that("mlr3verse random forest works correctly", {
    withr_library("marginaleffects")
    withr_library("mlr3verse")
    withr_library("fmeffects")
    withr_library("tidymodels")

    data("bikes", package = "fmeffects")
    bikes <<- bikes

    # fit model
    task <- as_task_regr(x = bikes, id = "bikes", target = "count")
    forest <- lrn("regr.ranger")$train(task)

    # Plot predictions
    p <- plot_predictions(forest, condition = "temp", type = "response", newdata = bikes)
    expect_s3_class(p, "gg")

    # Centered difference
    cmp <- avg_comparisons(forest, newdata = bikes, variables = c("temp", "season", "weather"))
    expect_s3_class(cmp, "comparisons")

    # Forward difference
    cmp <- avg_comparisons(
        forest,
        variables = list("temp" = \(x) data.frame(x, x + 1)),
        newdata = bikes
    )
    expect_s3_class(cmp, "comparisons")
})

test_that("mlr3verse cross-comparisons work correctly", {
    withr_library("marginaleffects")
    withr_library("mlr3verse")
    withr_library("fmeffects")

    data("bikes", package = "fmeffects")

    # fit model
    task <- as_task_regr(x = bikes, id = "bikes", target = "count")
    forest <- lrn("regr.ranger")$train(task)

    # Average effect of a simultaneous change in multiple variables
    cmp <- avg_comparisons(
        forest,
        variables = c("temp", "season", "weather"),
        cross = TRUE,
        newdata = bikes
    )
    expect_s3_class(cmp, "comparisons")
})

test_that("mlr3verse with tidymodels works correctly", {
    withr_library("marginaleffects")
    withr_library("mlr3verse")
    withr_library("fmeffects")
    withr_library("tidymodels")

    data("bikes", package = "fmeffects")

    # tidymodels
    forest_tidy <- rand_forest(mode = "regression") |>
        set_engine("ranger") |>
        fit(count ~ ., data = bikes)
    cmp <- avg_comparisons(
        forest_tidy,
        newdata = bikes,
        type = "numeric",
        variables = c("temp", "season", "weather")
    ) |>
        suppressWarnings()
    expect_s3_class(cmp, "comparisons")
})
