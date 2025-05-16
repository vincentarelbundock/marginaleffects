source("helpers.R")

requiet("mlr3verse")
requiet("fmeffects")
requiet("tidymodels")
data("bikes", package = "fmeffects")
bikes <<- bikes

# fit model
task <- as_task_regr(x = bikes, id = "bikes", target = "count")
forest <- lrn("regr.ranger")$train(task)

# Plot predictions
p <- plot_predictions(forest, condition = "temp", type = "response", newdata = bikes)
expect_inherits(p, "gg")

# Centered difference
cmp <- avg_comparisons(forest, newdata = bikes, variables = c("temp", "season", "weather"))
expect_inherits(cmp, "comparisons")

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
# expect_equivalent(effects$ame, cmp$estimate)

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
cmp <- avg_comparisons(forest_tidy, newdata = bikes, type = "numeric", variables = c("temp", "season", "weather"))
expect_inherits(cmp, "comparisons")
