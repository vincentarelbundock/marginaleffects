source("helpers.R")

# not supported because datplot is not representative, so we can't take minmax or iqr
mod <- lm(mpg ~ wt * hp, data = mtcars)
expect_error(plot_cco(mod, effect = list("hp" = "minmax"), condition = "wt"), pattern = "minmax")
expect_false(expect_error(plot_cco(mod, effect = list("hp" = c(100, 130)), condition = "wt")))

# one effect at a time
expect_error(plot_cco(mod, effect = c("hp", "wt"), condition = "wt"), pattern = "length")
