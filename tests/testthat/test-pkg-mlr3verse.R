testthat::skip_if_not_installed("mlr3verse")
testthat::skip_if_not_installed("fmeffects")
testthat::skip_if_not_installed("tidymodels")
requiet("mlr3verse")
requiet("fmeffects")
requiet("tidymodels")
data("bikes", package = "fmeffects")
bikes <<- bikes


# Basic expectation tests
task_simple <- mlr3::as_task_regr(mtcars, target = "mpg", id = "mtcars")
mod_simple <- mlr3::lrn("regr.lm")$train(task_simple)
expect_slopes2(mod_simple, variables = "hp", newdata = mtcars, se = FALSE)
expect_comparisons2(mod_simple, variables = "hp", newdata = mtcars, se = FALSE)
expect_predictions2(mod_simple, newdata = mtcars, se = FALSE)
expect_error(hypotheses(mod_simple), regexp = "does not support")


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

# fmeffects breaking change without warning or note in NEWS
# effects = fme(
#     # model = forest,
# data = bikes,
# target = "count",
# feature = "temp")
# step.size = 1)
# expect_equal(effects$ame, cmp$estimate, ignore_attr = TRUE)

# Average effect of a simultaneous change in multiple variables
cmp <- avg_comparisons(
    forest,
    variables = c("temp", "season", "weather"),
    cross = TRUE,
    newdata = bikes
)

# tidymodels
forest_tidy <- rand_forest(mode = "regression") |>
    set_engine("ranger") |>
    fit(count ~ ., data = bikes)
cmp <- avg_comparisons(forest_tidy,
    newdata = bikes,
    type = "numeric", variables = c("temp", "season", "weather")) |>
    suppressWarnings()
expect_s3_class(cmp, "comparisons")
