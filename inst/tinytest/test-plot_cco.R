source("helpers.R")

# not supported because datplot is not representative, so we can't take minmax or iqr
mod <- lm(mpg ~ wt * hp, data = mtcars)

p <- plot_cco(mod, effect = list(hp = "minmax"), condition = "wt", draw = FALSE)
expect_equivalent(length(unique(p$comparison)), 25)
p <- plot_cco(mod, effect = list(hp = "minmax"), condition = "wt")
expect_inherits(p, "gg")
p <- plot_cco(mod, effect = list(hp = "iqr"), condition = "wt")

p <- plot_cco(mod, effect = list("hp" = c(100, 130)), condition = "wt")
expect_inherits(p, "gg")

# one effect at a time
expect_error(plot_cco(mod, effect = c("hp", "wt"), condition = "wt"), pattern = "length")
