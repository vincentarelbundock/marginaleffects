source("helpers.R")
exit_if_not(require("tinyviztest"))
exit_if_not(!ON_OSX)
using("tinyviztest")
using("marginaleffects")

mod <- lm(mpg ~ hp * factor(gear), mtcars)
expect_snapshot_print(predictions(mod), "print-predictions")
expect_snapshot_print(predictions(mod, by = "gear"), "print-predictions_by")

# explicitly in datagrid means we care about it, so let's keep that
expect_snapshot_print(
    predictions(mod, newdata = datagrid(gear = unique)),
    "print-predictions_datagrid")

## guides()-related error in diffObj. Does not seem marginaleffects-related
# expect_snapshot_print(comparisons(mod), "print-comparisons")
expect_snapshot_print(comparisons(mod, by = "gear"), "print-comparisons_by")

expect_snapshot_print(marginal_means(mod, "gear"), "print-marginal_means")