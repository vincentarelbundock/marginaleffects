source("helpers.R")
exit_if_not(require("tinyviztest"))
using("marginaleffects")
exit_if_not(!ON_OSX)


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


exit_file("reinstate these tests once things are working")

# Issue #607: plot_slopes(mod)
mod <- glm(am ~ vs + qsec, data = mtcars, family = binomial)
expect_snapshot_plot(plot_comparisons(mod), "plot_comparisons-no_effect")
expect_snapshot_plot(plot_slopes(mod), "plot_slopes-no_effect")


rm(list = ls())