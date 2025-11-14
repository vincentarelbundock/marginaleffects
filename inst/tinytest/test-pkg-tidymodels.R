source("helpers.R")
using("marginaleffects")
requiet("tidymodels")
if (ON_CRAN || ON_CI) exit_file("local only")

# Basic expectation tests
mod_simple <- parsnip::linear_reg() |>
    parsnip::set_engine("lm") |>
    parsnip::fit(mpg ~ wt + am, data = mtcars)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

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

p <- predictions(mod, newdata = dat, type = "prob")
expect_inherits(p, "predictions")
expect_true("std.error" %in% colnames(p))

p <- predictions(mod, newdata = dat, type = "class")
expect_inherits(p, "predictions")
expect_false("std.error" %in% colnames(p))

mfx <- avg_slopes(mod, variables = c("bill_length_mm", "flipper_length_mm"), newdata = dat, type = "prob")
expect_inherits(mfx, "marginaleffects")
expect_true(nrow(mfx) > 0)


# conformal
set.seed(48103)
dat <- get_dataset("penguins", "palmerpenguins") |> na.omit()
dat <- dat[sample(1:nrow(dat), nrow(dat)), ] # shuffle species
mod <- set_engine(linear_reg(), "lm") |>
    fit(body_mass_g ~ bill_length_mm + flipper_length_mm + species,
        data = na.omit(dat))
p <- predictions(mod, newdata = dat[1:100, ]) |>
    inferences(
        R = 3,
        method = "conformal_cv+",
        data_train = dat[1:100, ],
        data_calib = dat[101:nrow(dat), ]
    )
expect_inherits(p, "predictions")

# workflow: engine supported
data("bikes", package = "fmeffects")

mod <- workflow(count ~ ., linear_reg()) |>
    fit(data = bikes) |>
    suppressWarnings()

p <- predictions(mod, newdata = bikes, type = "numeric") |>
    suppressWarnings()
expect_inherits(p, "predictions")
expect_true("std.error" %in% colnames(p))

p <- predictions(mod, newdata = bikes[1:200, ], vcov = FALSE) |>
    inferences(
        method = "conformal_split",
        data_train = bikes[1:200, ],
        data_calib = bikes[201:nrow(bikes), ])
expect_inherits(p, "predictions")

mfx <- avg_slopes(mod, variables = c("temp", "season", "weather"), newdata = bikes, type = "numeric") |>
    suppressWarnings()
expect_inherits(mfx, "marginaleffects")
expect_true(nrow(mfx) > 0)


# workflow: engine not supported
mod <- workflow(count ~ ., rand_forest(mode = "regression")) |>
    fit(data = bikes) |>
    suppressWarnings()

p <- predictions(mod, newdata = bikes, type = "numeric", vcov = FALSE)
expect_inherits(p, "predictions")
expect_false("std.error" %in% colnames(p))

mfx <- slopes(mod,
    variables = c("temp", "season", "weather"),
    newdata = bikes,
    type = "numeric",
    vcov = FALSE)
expect_inherits(mfx, "slopes")
expect_false("std.error" %in% colnames(mfx))


# Issue #1202
fit <- linear_reg() |>
    set_engine("lm") |>
    fit(hp ~ am * vs, data = mtcars)
p <- plot_predictions(fit, condition = "am", draw = FALSE)
expect_inherits(p, "data.frame")

p <- plot_comparisons(fit, variables = "am", condition = "vs", draw = FALSE)
expect_inherits(p, "data.frame")

p <- plot_comparisons(fit, variables = "am", condition = "vs", draw = FALSE)
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


# Bootstrap
set.seed(48103)
nobs <- 50
wf <- workflow() |>
    add_model(boost_tree(mode = "regression")) |>
    add_recipe(
        recipe(Sepal.Length ~ ., data = iris) |>
            # 1. Convert character predictors to factors (if any)
            step_string2factor(all_nominal_predictors()) |>
            # 2. Dummy-code all nominal predictors
            step_dummy(all_nominal_predictors())
    ) |>
    fit(iris)
mfx1 <- comparisons(wf, newdata = iris, variable = "Sepal.Width", vcov = FALSE)
mfx2 <- inferences(mfx1, R = 100, method = "rsample", data_train = iris) |>
    suppressWarnings()
expect_false("conf.low" %in% colnames(mfx1))
expect_true("conf.low" %in% colnames(mfx2))


# Bootstrap for some supported models but not all
z <- boost_tree("regression") |>
    fit(hp ~ ., data = mtcars)
comparisons(z, variables = "mpg", newdata = mtcars, vcov = FALSE) |>
    inferences(R = 10, method = "boot", data_train = mtcars) |>
    suppressWarnings() |>
    expect_error(pattern = "Failed to refit")
z <- linear_reg() |>
    fit(hp ~ ., data = mtcars)
cmp <- comparisons(z, variables = "mpg", newdata = mtcars, vcov = FALSE) |>
    inferences(R = 10, method = "boot", data_train = mtcars) |>
    suppressWarnings()
expect_inherits(cmp, "comparisons")
expect_true("conf.low" %in% colnames(cmp))
