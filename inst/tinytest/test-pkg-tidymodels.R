source("helpers.R")
using("marginaleffects")
requiet("tidymodels")
if (ON_CRAN || ON_CI) exit_file("local only")

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
    predictions(mod, type = "prob", newdata = dat) |> inferences(method = "conformal_cv+"), "does not support")

p <- predictions(mod, newdata = dat, type = "prob")
expect_inherits(p, "predictions")
expect_true("std.error" %in% colnames(p))

p <- predictions(mod, newdata = dat, type = "class")
expect_inherits(p, "predictions")
expect_false("std.error" %in% colnames(p))

mfx <- avg_slopes(mod, variables = c("bill_length_mm", "flipper_length_mm"), newdata = dat, type = "prob")
expect_inherits(mfx, "marginaleffects")
expect_true(nrow(mfx) > 0)


# workflow: engine supported
data("bikes", package = "fmeffects")

mod <- workflow(count ~ ., linear_reg()) |>
    fit(data = bikes) |>
    suppressWarnings()

p <- predictions(mod, newdata = bikes, type = "numeric") |>
    suppressWarnings()
expect_inherits(p, "predictions")
expect_true("std.error" %in% colnames(p))

mfx <- avg_slopes(mod,
    variables = c("temp", "season", "weather"),
    newdata = bikes, type = "numeric") |>
    suppressWarnings()
expect_inherits(mfx, "marginaleffects")
expect_true(nrow(mfx) > 0)


# workflow: engine not supported
mod <- workflow(count ~ ., rand_forest(mode = "regression")) |>
    fit(data = bikes) |>
    suppressWarnings()

p <- predictions(mod, newdata = bikes, type = "numeric")
expect_inherits(p, "predictions")
expect_false("std.error" %in% colnames(p))

mfx <- slopes(mod, variables = c("temp", "season", "weather"), newdata = bikes, type = "numeric")
expect_inherits(mfx, "slopes")
expect_false("std.error" %in% colnames(mfx))


# Issue #1202
fit <- linear_reg() |>
    set_engine("lm") |>
    fit(hp ~ am * vs, data = mtcars)
p <- plot_predictions(fit, condition = "am", draw = FALSE, newdata = mtcars)
expect_inherits(p, "data.frame")

p <- plot_comparisons(fit, variables = "am", condition = "vs", draw = FALSE, newdata = mtcars)
expect_inherits(p, "data.frame")

p <- plot_comparisons(fit, variables = "am", condition = "vs", draw = FALSE, newdata = mtcars)
expect_inherits(p, "data.frame")


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
expect_equivalent(mfx$x, my_data$x)
expect_equivalent(mfx$y, my_data$y)
