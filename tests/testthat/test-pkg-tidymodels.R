testthat::skip_if_not_installed("tidymodels")
requiet("tidymodels")
ON_CRAN <- !identical(Sys.getenv("NOT_CRAN"), "true")
ON_CI <- isTRUE(as.logical(Sys.getenv("CI")))
testthat::skip_if(ON_CRAN || ON_CI, "local only")

# Basic expectation tests
mod_simple <- parsnip::linear_reg() |>
    parsnip::set_engine("lm") |>
    parsnip::fit(mpg ~ wt + am, data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

dat_tidymodels <- get_dataset("penguins", "palmerpenguins")
dat_tidymodels$large_penguin <- ifelse(
    dat_tidymodels$body_mass_g > median(dat_tidymodels$body_mass_g, na.rm = TRUE),
    "yes",
    "no"
)
dat_tidymodels$large_penguin <- factor(dat_tidymodels$large_penguin, levels = c("yes", "no"))

# class
mod <- set_engine(logistic_reg(), "glm") |>
    fit(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat_tidymodels)

p <- predictions(mod, newdata = dat_tidymodels, type = "prob")
expect_s3_class(p, "predictions")
expect_true("std.error" %in% colnames(p))

p <- predictions(mod, newdata = dat_tidymodels, type = "class")
expect_s3_class(p, "predictions")
expect_false("std.error" %in% colnames(p))

mfx <- avg_slopes(mod, variables = c("bill_length_mm", "flipper_length_mm"), newdata = dat_tidymodels, type = "prob")
expect_s3_class(mfx, "marginaleffects")
expect_true(nrow(mfx) > 0)


# conformal
dat_tidymodels2 <- get_dataset("penguins", "palmerpenguins") |> na.omit()
mod <- set_engine(linear_reg(), "lm") |>
    fit(body_mass_g ~ bill_length_mm + flipper_length_mm + species,
        data = na.omit(dat_tidymodels2))
p <- predictions(mod, newdata = dat_tidymodels2[1:100, ]) |>
    inferences(
        R = 3,
        method = "conformal_cv+",
        data_calib = dat_tidymodels2[101:nrow(dat_tidymodels2), ]
    )
expect_s3_class(p, "predictions")

# workflow: engine supported
data("bikes", package = "fmeffects")

mod <- workflow(count ~ ., linear_reg()) |>
    fit(data = bikes) |>
    suppressWarnings()

p <- predictions(mod, newdata = bikes, type = "numeric") |>
    suppressWarnings()
expect_s3_class(p, "predictions")
expect_true("std.error" %in% colnames(p))

p <- predictions(mod, newdata = bikes[1:200, ], vcov = FALSE) |>
    inferences(
        method = "conformal_split",
        data_calib = bikes[201:nrow(bikes), ])
expect_s3_class(p, "predictions")

mfx <- avg_slopes(mod, variables = c("temp", "season", "weather"), newdata = bikes, type = "numeric") |>
    suppressWarnings()
expect_s3_class(mfx, "marginaleffects")
expect_true(nrow(mfx) > 0)


# workflow: engine not supported
mod <- workflow(count ~ ., rand_forest(mode = "regression")) |>
    fit(data = bikes) |>
    suppressWarnings()

p <- predictions(mod, newdata = bikes, type = "numeric", vcov = FALSE)
expect_s3_class(p, "predictions")
expect_false("std.error" %in% colnames(p))

mfx <- slopes(mod,
    variables = c("temp", "season", "weather"),
    newdata = bikes,
    type = "numeric",
    vcov = FALSE)
expect_s3_class(mfx, "slopes")
expect_false("std.error" %in% colnames(mfx))


# Issue #1202
fit <- linear_reg() |>
    set_engine("lm") |>
    fit(hp ~ am * vs, data = mtcars)
p <- plot_predictions(fit, condition = "am", draw = FALSE)
expect_s3_class(p, "data.frame")

p <- plot_comparisons(fit, variables = "am", condition = "vs", draw = FALSE)
expect_s3_class(p, "data.frame")

p <- plot_comparisons(fit, variables = "am", condition = "vs", draw = FALSE)
expect_s3_class(p, "data.frame")


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
