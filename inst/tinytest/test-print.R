source("helpers.R")
using("marginaleffects")
if (!requiet("tinysnapshot")) exit_file("tinysnapshot")
using("tinysnapshot")

mod <- lm(mpg ~ hp * factor(gear), mtcars)
expect_snapshot_print(predictions(mod), "print-predictions", ignore.white.space = TRUE)
expect_snapshot_print(predictions(mod, by = "gear"), "print-predictions_by", ignore.white.space = TRUE)

## guides()-related error in diffObj. Does not seem marginaleffects-related
# expect_snapshot_print(comparisons(mod), "print-comparisons")
expect_snapshot_print(comparisons(mod, by = "gear"), "print-comparisons_by")


# Issue #638: keep datagrid() explicit variables in print
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/Titanic.csv")
m <- glm(Survived ~ Age * PClass * SexCode, data = dat, family = binomial)
p <- predictions(m, newdata = datagrid(PClass = unique, SexCode = 0:1))
expect_snapshot_print(p, "print-predictions_datagrid")

# Issue #1270
mod <- lm(hp ~ mpg * factor(am), mtcars)
cmp <- avg_comparisons(mod, variables = "am", by = "am")
expect_snapshot_print(cmp, "print-comparisons_by_and_variables")

# twitter Kurz request
mod <- lm(mpg ~ hp * factor(am), mtcars)
expect_snapshot_print(
  comparisons(mod, variables = "am", newdata = data.frame(am = 0:1, hp = 120)),
  "print-comparisons_1focal_dataframe")

expect_snapshot_print(
  comparisons(mod, variables = "am", newdata = datagrid(am = 0:1, hp = 120)),
  "print-comparisons_1focal_datagrid")

expect_snapshot_print(
  predictions(mod, newdata = data.frame(am = 0:1, hp = 120)),
  "print-predictions_newdata")




rm(list = ls())
