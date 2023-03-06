source("helpers.R")
if (!requiet("tinysnapshot")) exit_file("tinysnapshot")
# if (ON_WINDOWS || ON_OSX) exit_file("linux only")
using("marginaleffects")


mod <- lm(mpg ~ wt * hp, data = mtcars)

p <- plot_comparisons(mod, variables = list(hp = "minmax"), condition = "wt", draw = FALSE)
expect_equivalent(length(unique(p$estimate)), 50)
p <- plot_comparisons(mod, variables = list(hp = "minmax"), condition = "wt")
expect_inherits(p, "gg")
p <- plot_comparisons(mod, variables = list(hp = "iqr"), condition = "wt")
p <- plot_comparisons(mod, variables = list("hp" = c(100, 130)), condition = "wt")
expect_inherits(p, "gg")


# representative values
p <- plot_comparisons(mod, variables = list(hp = "minmax"), condition = list("wt" = "threenum"))
expect_snapshot_plot(p, "plot_comparisons-minmax_x")

# two effects
p <- plot_comparisons(mod, variables = c("hp", "wt"), condition = "wt")
expect_snapshot_plot(p, "plot_comparisons-2effects")

# bug from examples (revise_get_data() no longer returns a factor attribute)
mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
p <- plot_comparisons(mod, variables = "hp", condition = list("am", "drat" = 3:5), draw = FALSE)
expect_inherits(p, "data.frame")


# Issue #592
mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
p <- plot_comparisons(mod, variables = "hp", condition = list("am", "drat" = 3:5))
expect_inherits(p, "gg")


# Issue #545: blank graph
library(ggplot2)
dat_titanic <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/Titanic.csv")
mod2 <- glm(Survived  ~ Age, data = dat_titanic, family = binomial)
p <- plot_comparisons(
    mod2,
    variables = list("Age" = 10),
    condition = "Age",
    comparison = "ratio") +
    ylab("Adjusted Risk Ratio\nP(Survived = 1 | Age + 10) / P(Survived = 1 | Age)")
expect_snapshot_plot(p, "plot_comparisons-rr_titanic")



rm(list = ls())