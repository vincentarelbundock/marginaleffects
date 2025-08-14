skip_on_cran()

test_that("tidymodels logistic regression works correctly", {
    withr_library("marginaleffects")
    withr_library("tidymodels")

    dat <- get_dataset("penguins", "palmerpenguins")
    dat$large_penguin <- ifelse(
        dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE),
        "yes",
        "no"
    )
    dat$large_penguin <- factor(dat$large_penguin, levels = c("yes", "no"))

    # class
    mod <- set_engine(logistic_reg(), "glm") |>
        fit(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat)

    # inferences() does not support tidymodels
    expect_error(
        predictions(mod, type = "prob", newdata = dat) |>
            inferences(method = "conformal_cv+"),
        "does not support"
    )

    p <- predictions(mod, newdata = dat, type = "prob")
    expect_s3_class(p, "predictions")
    expect_true("std.error" %in% colnames(p))

    p <- predictions(mod, newdata = dat, type = "class")
    expect_s3_class(p, "predictions")
    expect_false("std.error" %in% colnames(p))

    mfx <- avg_slopes(mod, variables = c("bill_length_mm", "flipper_length_mm"), newdata = dat, type = "prob")
    expect_s3_class(mfx, "marginaleffects")
    expect_true(nrow(mfx) > 0)
})

test_that("tidymodels workflow with supported engine works correctly", {
    withr_library("marginaleffects")
    withr_library("tidymodels")
    withr_library("fmeffects")

    data("bikes", package = "fmeffects")

    # workflow: engine supported
    mod <- workflow(count ~ ., linear_reg()) |>
        fit(data = bikes) |>
        suppressWarnings()

    p <- predictions(mod, newdata = bikes, type = "numeric") |>
        suppressWarnings()
    expect_s3_class(p, "predictions")
    expect_true("std.error" %in% colnames(p))

    mfx <- avg_slopes(mod, variables = c("temp", "season", "weather"), newdata = bikes, type = "numeric") |>
        suppressWarnings()
    expect_s3_class(mfx, "marginaleffects")
    expect_true(nrow(mfx) > 0)
})

test_that("tidymodels workflow with unsupported engine works correctly", {
    withr_library("marginaleffects")
    withr_library("tidymodels")
    withr_library("fmeffects")

    data("bikes", package = "fmeffects")

    # workflow: engine not supported
    mod <- workflow(count ~ ., rand_forest(mode = "regression")) |>
        fit(data = bikes) |>
        suppressWarnings()

    p <- predictions(mod, newdata = bikes, type = "numeric") |>
        suppressWarnings()
    expect_s3_class(p, "predictions")
    expect_false("std.error" %in% colnames(p))

    mfx <- slopes(mod, variables = c("temp", "season", "weather"), newdata = bikes, type = "numeric") |>
        suppressWarnings()
    expect_s3_class(mfx, "slopes")
    expect_false("std.error" %in% colnames(mfx))
})

test_that("tidymodels plotting functions work correctly", {
    withr_library("marginaleffects")
    withr_library("tidymodels")

    # Issue #1202
    fit <- linear_reg() |>
        set_engine("lm") |>
        fit(hp ~ am * vs, data = mtcars)
    p <- plot_predictions(fit, condition = "am", draw = FALSE, newdata = mtcars)
    expect_s3_class(p, "data.frame")

    p <- plot_comparisons(fit, variables = "am", condition = "vs", draw = FALSE, newdata = mtcars)
    expect_s3_class(p, "data.frame")

    p <- plot_comparisons(fit, variables = "am", condition = "vs", draw = FALSE, newdata = mtcars)
    expect_s3_class(p, "data.frame")
})

test_that("tidymodels with recipes works correctly", {
    withr_library("marginaleffects")
    withr_library("tidymodels")

    # Issue 1209
    nobs <- 50
    my_data <- tibble(
        x = runif(nobs, 0, 10),
        y = -(x - 11)^2 + 100 + rnorm(nobs, 0, 25)
    )
    lr_spec <- linear_reg()
    lr_rec <- recipe(y ~ x, data = my_data) |>
        step_poly(x, degree = 2)
    lr_wf <- workflow() |>
        add_model(lr_spec) |>
        add_recipe(lr_rec)
    lr_fit <- lr_wf |>
        fit(my_data)
    mfx <- slopes(lr_fit, newdata = my_data, variable = "x")
    expect_equal(mfx$x, my_data$x, ignore_attr = TRUE)
    expect_equal(mfx$y, my_data$y, ignore_attr = TRUE)
})
