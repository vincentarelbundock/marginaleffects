source("helpers.R")
exit_if_not(require("tinyviztest"))
using("marginaleffects")
exit_if_not(!ON_OSX)

# not supported because datplot is not representative, so we can't take minmax or iqr
mod <- lm(mpg ~ wt * hp, data = mtcars)

p <- plot_comparisons(mod, effect = list(hp = "minmax"), condition = "wt", draw = FALSE)
expect_equivalent(length(unique(p$estimate)), 25)
p <- plot_comparisons(mod, effect = list(hp = "minmax"), condition = "wt")
expect_inherits(p, "gg")
p <- plot_comparisons(mod, effect = list(hp = "iqr"), condition = "wt")

p <- plot_comparisons(mod, effect = list("hp" = c(100, 130)), condition = "wt")
expect_inherits(p, "gg")

# one effect at a time
expect_error(plot_comparisons(mod, effect = c("hp", "wt"), condition = "wt"), pattern = "length")


# bug from examples (revise_get_data() no longer returns a factor attribute)
mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
p <- plot_comparisons(mod, effect = "hp", condition = list("am", "drat" = 3:5), draw = FALSE)
expect_inherits(p, "data.frame")


# Issue #592
mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
p <- plot_comparisons(mod, effect = "hp", condition = list("am", "drat" = 3:5))
expect_inherits(p, "gg")



# Issue #607: plot_slopes(mod)
mod <- glm(am ~ vs + qsec, data = mtcars, family = binomial)
expect_snapshot_plot(plot_comparisons(mod), "plot_comparisons-no_effect")
expect_snapshot_plot(plot_slopes(mod), "plot_slopes-no_effect")