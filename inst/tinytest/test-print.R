source("helpers.R")
using("marginaleffects")
if (!requiet("tinysnapshot")) exit_file("tinysnapshot")
using("tinysnapshot")

mod <- lm(mpg ~ hp * factor(gear), mtcars)
expect_snapshot_print(predictions(mod), "print-predictions")
expect_snapshot_print(predictions(mod, by = "gear"), "print-predictions_by")

## guides()-related error in diffObj. Does not seem marginaleffects-related
# expect_snapshot_print(comparisons(mod), "print-comparisons")
expect_snapshot_print(comparisons(mod, by = "gear"), "print-comparisons_by")

expect_snapshot_print(marginal_means(mod, "gear"), "print-marginal_means")


# Issue #638: keep datagrid() explicit variables in print
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/Titanic.csv")
m <- glm(Survived ~ Age * PClass * SexCode, data = dat, family = binomial)
p <- predictions(m, newdata = datagrid(PClass = unique, SexCode = 0:1))
expect_snapshot_print(p, "print-predictions_datagrid")


# twitter Kurz request
mod <- lm(mpg ~ hp + am, data = mtcars)
    comparisons(mod, variables = "am", newdata = data.frame(am = 0:1, hp = 120))
expect_snapshot_print(
    comparisons(mod, variables = "am", newdata = data.frame(am = 0:1, hp = 120)),
    "print-comparisons_1focal")

expect_snapshot_print(
    predictions(mod, newdata = data.frame(am = 0:1, hp = 120)),
    "print-predictions_newdata")


rm(list = ls())